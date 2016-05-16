library(WDI)
library(sqldf)


#### Collect the Relevant WDI Indicaters #### 
# "SP.POP.TOTL" - Total Population
# "VC.IHR.PSRC.P5" - Homicides "VC.IHR.PSRC.P5"
#  "SP.POP.GROW" - PopulationGrowth 
# "NY.GDP.MKTP.KD.ZG- GDPGrowth
# "NY.GNP.PCAP.CD GNIPC - GNI Percapita 



data <- WDI(country = "all", indicator = c("SP.POP.TOTL","VC.IHR.PSRC.P5",
                                           "SP.POP.GROW","NY.GDP.MKTP.KD.ZG","NY.GNP.PCAP.CD"),
            start = 1990, end = 2015, extra = FALSE, cache = NULL)

#### Prepare the Dataframe for Analysis #### 

cnames <- c("Code","Country","Year","Population","Homicides","PopulationGrowth",
              "GDPGrowth","GNIPC")
colnames(data) <- cnames


TriangleData <- sqldf("SELECT * FROM data WHERE (Country = 'El Salvador'
                      OR Country = 'Guatemala' OR Country = 'Honduras')
                      AND (Year > 2001 AND Year < 2014)
                      ORDER BY Country,Year ASC")

aliens <- read.csv("aliens.csv")
TriangleData$Aliens <- aliens$Aliens
TriangleData$OneHundredK <-  (TriangleData$Population / 100000)
TriangleData$Aliens100K <- (TriangleData$Aliens / TriangleData$OneHundredK)
#### Output the Data head here ####
head(TriangleData)

# There is a NULL Value for 2013 in Guetamala, 
TriangleData[24,]$Homicides <- 34.57425

#### Run Regresssion Analysis #### 
emigrationModel <- lm(TriangleData$Aliens100K ~ TriangleData$PopulationGrowth
                       + TriangleData$GDPGrowth + TriangleData$GNIPC + TriangleData$Homicides)

summary(emigrationModel)

# Analysis the Summary

#### Make Prediction and Plot #### 
testData <- sqldf("SELECT PopulationGrowth,GDPGrowth,GNIPC,Homicides
                  FROM TriangleData")
predictedAliens <- predict(emigrationModel,testData$OneHundredK)


# Build Plot 
plot(predictedAliens,TriangleData$Aliens100K,
     main="Predicted vs Detentions per Capita",
     xlab="Actual",
     ylab="Predicted")

?citation