library(sqldf)
library(caret)
setwd("C:\\Users\\aadlandma\\Desktop\\one_sided_violence")
data <- read.csv("predictedRights3.csv")
d14  <- sqldf("SELECT * FROM data WHERE year = 2014")
barchart(table(d14$score),main="PTS Scores 2014",ylab="PTS Score",xlab="Count")
d19 <- sqldf("SELECT DISTINCT * FROM data WHERE year = 2019")
barchart(table(d19$score),main="PTS Scores 2019",ylab="PTS Score",xlab="Count")

d1419 <- sqldf("SELECT d14.country,d14.score as pts14,d19.score as pts19
               FROM d14
               JOIN d19 
               on d14.country = d19.country")