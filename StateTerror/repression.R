library(WDI)
library(sqldf)
library(countrycode)
library(MASS)
library(caret)
library(nnet)
library(randomForest)
setwd("C:\\Users\\aadlandma\\Desktop\\one_sided_violence")

#### Data to include ####
# homicides VC.IHR.PSRC.P5
# youth unemployment "SL.UEM.1524.ZS" 
# infant mortailiy "SP.DYN.IMRT.IN"
idx <- c("UPP.REV.POL.XQ","VC.IHR.PSRC.P5","SL.UEM.1524.ZS",
  "SP.DYN.IMRT.IN","GV.RULE.LW.ES","MO.INDEX.SRLW.XQ","RL.EST",
  "CC.EST","GV.TI.SCOR.IDX","IC.FRM.COR.ZS")

names <- c("iso2c","country","year","Polity","Homicides","YouthUnemployment",
           "InfantMortality","RuleLawEstimate","SafetyRuleLaw","RuleLawEst2","CorruptionControl",
           "CorruptionPerception","CorruptionConstraint")


indicators <- WDIsearch(string = "corruption", field = "name", short = TRUE,
                  cache = NULL)

indicators <- as.data.frame(indicators)

data2 <- WDI(country = "all", indicator = idx ,
             start = 1990, end = 2015, extra = FALSE, cache = NULL)

colnames(data) <- names
data$COW <- countrycode(data$iso2c,"iso2c","cown")

#### Ingest ####
polity <- read.csv("polity.csv")
pts <- read.csv("PTS2015.csv")


#### Append Data ####

PolityPts <- sqldf("SELECT polity.*,pts.state
                   FROM polity
                   JOIN pts
                   ON pts.COW = polity.ccode
                   AND pts.year = polity.year")
dataShort <- data[,c(1,2,3,6,7,10,11,14)]
PolityPts$Binary <- ifelse(PolityPts$state > 2,1,0)

#### Append WDI Data ####
df <- sqldf("SELECT PolityPts.*,dataShort.YouthUnemployment,dataShort.InfantMortality,
            dataShort.RuleLawEst2,datashort.CorruptionControl
            FROM PolityPts
            LEFT JOIN dataShort
            ON  dataShort.year = PolityPts.year
            AND dataShort.COW = PolityPts.ccode")

dfShort <- df[,c(3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,37,38,39,40,41,42)]
#### Model It ####
model <- glm(Binary ~ polity2 + durable + xrreg + xrcomp
             + xropen + xconst + exrec  + YouthUnemployment + InfantMortality 
             + RuleLawEst2 + CorruptionControl,
             family=binomial(link='logit'),data=dfComplete2)
summary(model)


r2 <- 1 - (log(1805.2) / log(2994.6))

dfComplete2 <- dfShort[which(complete.cases(dfShort)),]
d <- unique(dfComplete$scode)
d <- as.data.frame(countrycode(d,"cowc","iso2c"))
e <- as.data.frame(unique(dfComplete$year))
eet <- sqldf("SELECT * FROM data where iso2c in d and year in e")

dfComplete2 <- dfComplete2[which(complete.cases(dfComplete2)),]
#### Random Forest Model ####
class2ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}


training.rows <- createDataPartition(dfComplete2$Binary, 
                                     p = 0.8, list = FALSE)
train.batch <- dfComplete2[training.rows, ]
test.batch <- dfComplete2[-training.rows, ]


#minus triangle 
triangleTest <- sqldf("SELECT * FROM dfComplete2 WHERE country = 'Honduras'
                      OR country = 'El Salvador' or country = 'Guatemala'")

train.batch <- dfComplete2[-which(dfComplete2$country == "Honduras"|
                                    dfComplete2$country == "El Salvador" |
                                   dfComplete$country == 'Guatemala'),]

set.seed(35)
rf.grid <- data.frame(.mtry = c(1,2))


rf.tune <- train(as.factor(Binary) ~ as.factor(polity2) + durable + YouthUnemployment
                 + InfantMortality + RuleLawEst2 + CorruptionControl,
                data = train.batch,
                method = "rf", 
                tuneGrid = data.frame(mtry = 2))

rf.pred <- predict(rf.tune, triangleTest)
confusionMatrix(rf.pred, triangleTest$Binary)


#### MultiClass ####
mutli.tune <- randomForest(as.factor(state) ~ 
                             polity2 + durable
                             + YouthUnemployment
                             + InfantMortality + RuleLawEst2 
                             + CorruptionControl,
                             data=train.batch,ntree=1000,
                             mtry=2, importance=TRUE)

multi.pred <- predict(mutli.tune, triangleTest)
triangleTest$pred <- multi.pred 