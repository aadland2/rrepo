library(WDI)
library(sqldf)
library(countrycode)
library(MASS)
library(caret)
library(nnet)
library(randomForest)
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
mutli.tune <- randomForest(as.factor(state) ~ 
                             polity + durable
                           + YouthUnemployment
                           + InfantMortality + RuleLawEst2 
                           + CorruptionControl + CorruptionControl
                           + Population + GNIPerCapita,
                           data=train.batch,ntree=3000,
                           mtry=c(1,2), importance=TRUE)

multi.pred <- predict(mutli.tune, triangleTest)
triangleTest$pred <- multi.pred 
table(triangleTest$state,triangleTest$pred)

best <- triangleTest