library(WDI)
library(sqldf)
library(countrycode)
library(MASS)
library(caret)
library(nnet)
library(randomForest)
library(dplyr)
library(forecast)
library(corrplot)

setwd("C:\\Users\\aadlandma\\Desktop\\one_sided_violence")
#### Data to include ####
# homicides VC.IHR.PSRC.P5
# youth unemployment "SL.UEM.1524.ZS" 
# infant mortailiy "SP.DYN.IMRT.IN"
idx <- c("SL.UEM.1524.ZS",
         "SP.DYN.IMRT.IN","RL.EST",
         "CC.EST","SP.POP.TOTL","NY.GNP.PCAP.CD")

names <- c("iso2c","country","year","YouthUnemployment",
           "InfantMortality","RuleLawEst2",
           "CorruptionControl","Population","GNIPerCapita")

data <- WDI(country = "all", indicator = idx ,
             start = 1990, end = 2015, extra = FALSE, cache = NULL)

colnames(data) <- names
data$COW <- countrycode(data$iso2c,"iso2c","cown")

#### Polity Ingest ####
polity <- read.csv("polity.csv")
pts <- read.csv("PTS2015.csv")
colnames(pts) <- c("Country","COWalpha","COWnum",
                   "WorldBank","Year","Amnesty","state","HRW")
#### Append Data ####
PolityPts <- sqldf("SELECT polity.*,pts.state
                   FROM polity
                   JOIN pts
                   ON pts.COWnum = polity.ccode
                   AND pts.year = polity.year")

PolityPts$Binary <- ifelse(PolityPts$state > 2,1,0)

df2 <- sqldf("SELECT PolityPts.*,data.YouthUnemployment,data.InfantMortality,
            data.RuleLawEst2,data.CorruptionControl,data.Population,data.GNIPerCapita
            FROM PolityPts
            LEFT JOIN data
            ON  data.year = PolityPts.year
            AND data.COW = PolityPts.ccode")

dfShort <- sqldf("SELECT country,year,Binary,state,polity,durable,
                 YouthUnemployment,InfantMortality,RuleLawEst2,CorruptionControl,
                 Population,GNIPerCapita
                 FROM df2")
dfShortComplete <- dfShort[which(complete.cases(dfShort)),]

#### Random Forest Model ####
triangleTest <- sqldf("SELECT * FROM dfShortComplete WHERE country = 'Honduras'
                      OR country = 'El Salvador' or country = 'Guatemala'")

train.batch <- dfShortComplete[-which(dfShortComplete$country == "Honduras"|
                                    dfShortComplete$country == "El Salvador" |
                                    dfShortComplete$country == 'Guatemala'),]


rf.tune <- train(as.factor(Binary) ~ as.factor(polity) + durable + YouthUnemployment
                 + InfantMortality + RuleLawEst2 + CorruptionControl,
                 data = train.batch,
                 method = "rf", 
                 tuneGrid = data.frame(mtry = 2))

rf.pred <- predict(rf.tune, triangleTest)
confusionMatrix(rf.pred, triangleTest$Binary)


#### Seperate Test Set ####
training.rows <- createDataPartition(train.batch$Binary, 
                                     p = 0.8, list = FALSE)
train_set <- dfShortComplete[training.rows, ]
test_set <- dfShortComplete[-training.rows, ]

multi.tune <- randomForest(as.factor(state) ~ 
                             polity + durable
                           + YouthUnemployment
                           + InfantMortality + RuleLawEst2 
                           + CorruptionControl + CorruptionControl
                           + Population + GNIPerCapita,
                           data=train_set,ntree=3000,
                           mtry=c(1,2), importance=TRUE)

multi.pred <- predict(multi.tune, test_set)

#### Examine Model Performance ####
# Ovearll accuracy 
accuracy <- length(which(multi.pred == test_set$state)) / length(multi.pred)

# relative matches 
matches <- cor(table(test_set$state,multi.pred))
corrplot(matches,method="square")

#### Performance on Northern Triangle  ####
triangle.tune <- randomForest(as.factor(state) ~ 
                             polity + durable
                           + YouthUnemployment
                           + InfantMortality + RuleLawEst2 
                           + CorruptionControl + CorruptionControl
                           + Population + GNIPerCapita,
                           data=train.batch,ntree=3000,
                           mtry=c(1,2), importance=TRUE)

triangle.pred <- predict(triangle.tune, triangleTest)
triangleTest$pred <- triangle.pred 
table(triangleTest$state,triangleTest$pred)

#### Metrics ####
# Exact matches 
triangle.accuracy <- (length(which(triangle.pred  == triangleTest$state)) 
        / length(triangle.pred))

# relative matches 
triangle.matches <- cor(table(triangleTest$state,triangle.pred))
corrplot(triangle.matches,method="square")


#### Seperate out the three counties #### 
Honduras <- sqldf("SELECT * FROM triangleTest WHERE country = 'Honduras'
                  AND year > 1998")

ElSalvador <- sqldf("SELECT * FROM triangleTest WHERE country = 'El Salvador'
                  AND year > 1998")

Guatemala <- sqldf("SELECT * FROM triangleTest WHERE country = 'Guatemala'
                  AND year > 1998")

#### Build a function to forecast the values three years out ####
c <- Honduras$YouthUnemployment[0:13]
d <- Honduras$durable[0:13]
arima(c,c(0, 0,3))

pred <- predict(arima(c,c(0, 0,3)), n.ahead = 5, newxreg = NULL,
        se.fit = TRUE)

plot(pred$pred)

predict(arima(ElSalvador$durable,c(0,0,3)),n.ahead = 5, newxreg = NULL,
        se.fit = TRUE)

pred <- predict(auto.arima(d), n.ahead = 5, newxreg = NULL,
                se.fit = TRUE)

ddply(triangleTest,"country",function(d){

    #### parameters ####
    
    year <- c(2015:2019)

    ## Constants ##
    country <- rep(d$country[length(d$country)],times = length(year))
    polity <- rep(d$polity[length(d$polity)],times = length(year))
    durable <- d$durable[length(d$durable)] + c(1,2,3,4,5)

    ## Variables ## 
    YouthUnemployment <- predict(auto.arima(d$YouthUnemployment), n.ahead = 5, newxreg = NULL,
                          se.fit = TRUE)
    InfantMortality <- predict(auto.arima(d$InfantMortality), n.ahead = 5, newxreg = NULL,
                        se.fit = TRUE)

    RuleLawEst2 <- predict(auto.arima(d$RuleLawEst2), n.ahead = 5, newxreg = NULL,
                          se.fit = TRUE)

    CorruptionControl <- predict(auto.arima(d$CorruptionControl), n.ahead = 5, newxreg = NULL,
                          se.fit = TRUE)


    ## World Bank seems to be imputing the data with a linear model ##
    # Generate Population
    PopModel <- lm(d$Population ~ d$year)
    Population <- (PopModel$coefficients[2] * year + PopModel$coefficients[1])
    # Generate GNI
    GNIModel <- lm(d$GNIPerCapita ~ d$year)
    GNIPerCapita <- (GNIModel$coefficients[2] * year + GNIModel$coefficients[1])
    return()
})