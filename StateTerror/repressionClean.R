library(WDI)
library(sqldf)
library(countrycode)
library(MASS)
library(caret)
library(nnet)
library(randomForest)
library(plyr)
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


triangleTest <- sqldf("SELECT country,year,Binary,state,
                      polity,durable,YouthUnemployment,
                      InfantMortality,RuleLawEst2,
                      CorruptionControl,Population,
                      GNIPerCapita from dfShortComplete
                      WHERE country = 'Honduras'
                      OR country = 'El Salvador' or country = 'Guatemala'")

train_batch <- dfShortComplete[-which(dfShortComplete$country == "Honduras"|
                                    dfShortComplete$country == "El Salvador" |
                                    dfShortComplete$country == 'Guatemala'),]


train.batch <- sqldf("SELECT country,year,Binary,state,
                      polity,durable,YouthUnemployment,
                      InfantMortality,RuleLawEst2,
                      CorruptionControl,Population,
                      GNIPerCapita from train_batch")

train.batch$durable <- as.numeric(as.character(train.batch$durable))
triangleTest$durable <- as.numeric(as.character(triangleTest$durable))

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
                           data=train_set,ntree=1000,
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
                           + CorruptionControl 
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

#### Build a function to forecast the values three years out ####
triangle <- sqldf("SELECT * from triangleTest where year > 2000 and year < 2014")

triangleFuture <- ddply(triangle,~ country,function(d){
    #### parameters ####
    year <- c(2014:2018)
   
    ## Constants ##
    country <- rep(d$country[length(d$country)],times = length(year))
    polity <- rep(d$polity[length(d$polity)],times = length(year))
    durable <- d$durable[length(d$durable)] + c(1,2,3,4,5)
    
    ## Linear Models ## 
    YouthUnemploymentModel <- lm(d$YouthUnemployment ~ d$year)
    InfantMortalityModel <- lm(d$InfantMortality ~ d$year)
    RuleLawEst2Model <- lm(d$RuleLawEst2 ~ d$year)
    CorruptionControModel <- lm(d$CorruptionControl ~ d$year)
    

    #### Extract out the preds #### 
    YouthUnemployment <- (YouthUnemploymentModel$coefficients[2] * year + YouthUnemploymentModel$coefficients[1])
    InfantMortality <- (InfantMortalityModel$coefficients[2] * year + InfantMortalityModel$coefficients[1])
    RuleLawEst2 <-(RuleLawEst2Model$coefficients[2] * year + RuleLawEst2Model$coefficients[1])
    CorruptionControl <- (CorruptionControModel$coefficients[2] * year + CorruptionControModel$coefficients[1])
    
    ## World Bank seems to be imputing the data with a linear model ##
    # Generate Population
    PopModel <- lm(d$Population ~ d$year)
    Population <- (PopModel$coefficients[2] * year + PopModel$coefficients[1])
    # Generate GNI
    GNIModel <- lm(d$GNIPerCapita ~ d$year)
    GNIPerCapita <- (GNIModel$coefficients[2] * year + GNIModel$coefficients[1])
    df <- as.data.frame((cbind(country,year,polity,durable,YouthUnemployment,
                 InfantMortality,RuleLawEst2,CorruptionControl,Population,GNIPerCapita)))
    
    return(df)
})

# define a function to convert factors 
converter <- function(d){
  d$durable <- as.numeric(as.character(d$durable))
  d$polity <- as.numeric(as.character(d$polity))
  d$YouthUnemployment <- as.numeric(as.character(d$YouthUnemployment))
  d$InfantMortality <- as.numeric(as.character(d$InfantMortality))
  d$RuleLawEst2 <- as.numeric(as.character(d$RuleLawEst2))
  d$CorruptionControl <- as.numeric(as.character(d$CorruptionControl))
  d$Population <- as.numeric(as.character(d$Population))
  d$GNIPerCapita <- as.numeric(as.character(d$GNIPerCapita))
  return(d)
}
d <- converter(triangleFuture)

#### Validate for the Triangle Forecast 
triangleFuturePred <- predict(triangle.tune, d)
triangleFuture$forecast <- as.numeric(as.character(triangleFuturePred))
triangle2014 <- sqldf("SELECT * FROM triangleTest where year = 2014 ORDER BY country ASC")
triangleFuture2014 <- sqldf("SELECT * FROM triangleFuture where year = 2014")
table(triangle2014$state,triangleFuture2014$forecast)

#### Create a sepearte forecast just on the future variable ####
futureFrame <- sqldf('SELECT * from train_batch where year > 2000 and year < 2009
                     AND country in ("Albania","Algeria","Argentina","Armenia", 
"Australia","Austria","Azerbaijan","Bahrain", 
"Bangladesh","Belarus","Belgium","Benin", 
                     "Bhutan", "Bolivia","Bosnia", "Botswana",
                     "Brazil", "Bulgaria", "Burkina Faso","Burundi", 
                     "Cambodia", "Cameroon", "Canada", "Cape Verde",
                     "Central African Republic","Chad",   "Chile",  "China", 
                     "Colombia", "Comoros","Congo Brazzaville", "Congo Kinshasa",   
                     "Costa Rica","Croatia","Cyprus", "Czech Republic",   
                     "Denmark","Dominican Republic","East Timor","Ecuador","Egypt",  "Equatorial Guinea", "Estonia","Fiji",  
                     "Finland","France", "Gabon",  "Gambia",
                     "Georgia","Germany","Ghana",  "Greece",
                     "Guinea", "Guinea-Bissau", "Guyana", "Hungary", 
                     "India",  "Indonesia","Iran",   "Ireland", 
                     "Israel", "Italy",  "Ivory Coast","Jamaica", 
                     "Japan",  "Jordan", "Kazakhstan","Kenya", 
                     "Korea South","Kuwait", "Kyrgyzstan","Laos",  
                     "Latvia", "Lebanon","Lesotho","Liberia", 
                     "Libya",  "Lithuania","Luxembourg","Macedonia",
                     "Madagascar","Malawi", "Malaysia", "Mali",  
                     "Mauritania","Mauritius","Mexico", "Moldova", 
                     "Mongolia", "Morocco","Mozambique","Namibia", 
                     "Nepal",  "Netherlands","New Zealand","Nicaragua",
                     "Niger",  "Nigeria","Norway", "Oman",  
                     "Pakistan", "Panama", "Papua New Guinea",  "Paraguay",
                     "Peru",   "Philippines","Poland", "Portugal",
                     "Romania","Russia", "Rwanda", "Saudi Arabia", 
                     "Senegal","Sierra Leone","Singapore","Slovak Republic",  
                     "Slovenia", "Solomon Islands",   "South Africa","Spain", 
                     "Sri Lanka","Suriname", "Swaziland","Sweden",
                     "Switzerland","Tajikistan","Tanzania", "Thailand",
                     "Togo", "Trinidad and Tobago","Tunisia","Turkey",
                     "Turkmenistan","UAE","Uganda", "Ukraine", 
                     "United Kingdom","Uruguay","Uzbekistan","Yemen", 
                     "Zambia")')
futureFrame <- ddply(futureFrame,~ country,function(d){
  #### parameters ####
  year <- c(2009:2014)
  
  ## Constants ##
  country <- rep(d$country[length(d$country)],times = length(year))
  polity <- rep(d$polity[length(d$polity)],times = length(year))
  durable <- d$durable[length(d$durable)] + c(1,2,3,4,5)
  
  ## Linear Models ## 
  YouthUnemploymentModel <- lm(d$YouthUnemployment ~ d$year)
  InfantMortalityModel <- lm(d$InfantMortality ~ d$year)
  RuleLawEst2Model <- lm(d$RuleLawEst2 ~ d$year)
  CorruptionControModel <- lm(d$CorruptionControl ~ d$year)
  
  
  #### Extract out the preds #### 
  YouthUnemployment <- (YouthUnemploymentModel$coefficients[2] * year + YouthUnemploymentModel$coefficients[1])
  InfantMortality <- (InfantMortalityModel$coefficients[2] * year + InfantMortalityModel$coefficients[1])
  RuleLawEst2 <-(RuleLawEst2Model$coefficients[2] * year + RuleLawEst2Model$coefficients[1])
  CorruptionControl <- (CorruptionControModel$coefficients[2] * year + CorruptionControModel$coefficients[1])
  
  ## World Bank seems to be imputing the data with a linear model ##
  # Generate Population
  PopModel <- lm(d$Population ~ d$year)
  Population <- (PopModel$coefficients[2] * year + PopModel$coefficients[1])
  # Generate GNI
  GNIModel <- lm(d$GNIPerCapita ~ d$year)
  GNIPerCapita <- (GNIModel$coefficients[2] * year + GNIModel$coefficients[1])
  df <- as.data.frame((cbind(country,year,polity,durable,YouthUnemployment,
                             InfantMortality,RuleLawEst2,CorruptionControl,Population,GNIPerCapita)))
  
  return(df)
})

#### Predict on the Future Data
futurFrame <- converter(futureFrame)
futureFrameComplete <- futureFrame[which(complete.cases(futureFrame)),]
d <- converter(futureFrameComplete)
futureFrameComplete$pred <- predict(triangle.tune, d)

evaluateImputed <- sqldf("SELECT train_batch.country,train_batch.year,futureFrameComplete.pred,
                      train_batch.state
                      FROM futureFrameComplete
                      JOIN train_batch
                      ON futureFrameComplete.year = train_batch.year
                      AND futureFrameComplete.country = train_batch.country")

table(evaluateImputed$pred,evaluateImputed$state)
overallAccuracy <- length(which(evaluateImputed$pred == evaluateImputed$state)) / length(evaluateImputed$pred)
relativeAccuracy <- table(evaluateImputed$pred,evaluateImputed$state)
corrplot(matches,method="square")



#### Model Evaluation Move Later #### 
# Chart for distributions of data
length(unique(train.batch$country)) # number of countries 
barchart(table(train.batch$state),xlab="Country Years",ylab="PTS Score",main="PTS Distribution: Training")

# Test Set
barchart(table(test_set$state),xlab="Country Years",ylab="PTS Score",main="PTS Distribution: Test")
length(unique(test_set$country))

# Accuracy measurements
# Ovearll accuracy 
accuracy <- length(which(multi.pred == test_set$state)) / length(multi.pred)

# relative matches 
(table(test_set$state,multi.pred))
matches <- cor(table(test_set$state,multi.pred))
corrplot(matches,method="square")
