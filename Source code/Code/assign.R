install.packages("ggplot2")       
install.packages("GGally")
library("ggplot2")                     
library("GGally")
library(readr)
# import data
setwd("D:/Documents/university/sophomore/xstk/btl")
data <- read.csv("LocalCrimeOneYearofData2012.csv", header = TRUE)
view(data)

dim(data)

summary(data)

drop <- c("State","Months")
clearData = data[,!(names(data) %in% drop)]
clearData

head(clearData)

summary(clearData)

#boxplot
options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(1,1))
boxplot(clearData$Population, horizontal = TRUE, main = "Population", 
        col = "green")
boxplot(clearData$Violent_crime_total, horizontal = TRUE, 
        main = "Violent_crime_total", col = "antiquewhite")
boxplot(clearData$Murder_and_Manslaughter, horizontal = TRUE, 
        main = "Murder_and_Manslaughter", col = "antiquewhite3")
boxplot(clearData$Forcible_rape, horizontal = TRUE, main = "Forcible_rape", 
        col = "azure4")
boxplot(clearData$Robbery, horizontal = TRUE, main = "Robbery", col = "blue")
boxplot(clearData$Aggravated_assault, horizontal = TRUE, 
        main = "Aggravated_assault", col = "blue4")
boxplot(clearData$Property_crime_total, horizontal = TRUE, 
        main = "Property_crime_total", col = "brown")
boxplot(clearData$Burglary, horizontal = TRUE, main = "Burglary", 
        col = "burlywood")
boxplot(clearData$Larceny_theft, horizontal = TRUE, main = "Larceny_theft", 
        col = "burlywood4")
boxplot(clearData$lat, horizontal = TRUE, main = "lat", col = "chartreuse")
boxplot(clearData$long, horizontal = TRUE, main = "long", col = "chocolate1")

options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(1,1))
boxplot(clearData$Population ~ clearData$Violent_crime_total, 
        horizontal = TRUE, main = "Violent_crime_total-Population", 
        col = "green")
boxplot(clearData$Murder_and_Manslaughter ~ clearData$Forcible_rape, 
        horizontal = TRUE, main = "Murder_and_Manslaughter-Forcible_rape", 
        col = "antiquewhite")
boxplot(clearData$Forcible_rape ~ clearData$Robbery, 
        horizontal = TRUE, main = "Forcible_rape-Robbery", 
        col = "antiquewhite3")
boxplot(clearData$Robbery ~ clearData$Aggravated_assault, 
        horizontal = TRUE, main = "Robbery-Aggravated_assault", 
        col = "azure4")
boxplot(clearData$Aggravated_assault ~ clearData$Property_crime_total, 
        horizontal = TRUE, main = "Aggravated_assault-Property_crime_total", 
        col = "brown")
boxplot(clearData$Property_crime_total ~ clearData$Burglary, 
        horizontal = TRUE, main = "Property_crime_total-Burglary", 
        col = "burlywood")
boxplot(clearData$Burglary ~ clearData$Larceny_theft, 
        horizontal = TRUE, main = "Burglary-Larceny_theft", 
        col = "burlywood4")
boxplot(clearData$Larceny_theft ~ clearData$Motor_vehicle_theft, 
        horizontal = TRUE, main = "Larceny_theft-Motor_vehicle_theft", 
        col = "chartreuse")
boxplot(clearData$lat ~ clearData$long, 
        horizontal = TRUE, main = "lat-long", 
        col = "chocolate1")

#hist
options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(1,1))
hist(clearData$Population, main = "Population", col = "green")
hist(clearData$Violent_crime_total, main = "Violent_crime_total", 
     col = "antiquewhite")
hist(clearData$Murder_and_Manslaughter, main = "Murder_and_Manslaughter", 
     col = "antiquewhite3")
hist(clearData$Forcible_rape, main = "Forcible_rape", col = "azure4")
hist(clearData$Robbery, main = "Robbery", col = "brown")
hist(clearData$Aggravated_assault, main = "Aggravated_assault", 
     col = "burlywood")
hist(clearData$Property_crime_total, main = "Property_crime_total", 
     col = "burlywood4")
hist(clearData$Burglary, main = "Burglary", col = "chartreuse")
hist(clearData$Larceny_theft, main = "Larceny_theft", col = "chocolate1")
hist(clearData$Larceny_theft, main = "Larceny_theft", col = "cornflowerblue")

#qqnorm and qqline
options(repr.plot.width=30, repr.plot.height=8)
par(mfrow=c(1,1))
qqnorm(clearData$Violent_crime_total, frame = TRUE, 
       main = "Violent_crime_total")
qqline(clearData$Violent_crime_total, col = "steelblue", lwd = 2)

qqnorm(clearData$Murder_and_Manslaughter, frame = TRUE, 
       main = "Murder_and_Manslaughter")
qqline(clearData$Murder_and_Manslaughter, col = "steelblue", lwd = 2)

qqnorm(clearData$Forcible_rape, frame = TRUE, 
       main = "Forcible_rape")
qqline(clearData$Forcible_rape, col = "steelblue", lwd = 2)

qqnorm(clearData$Robbery, frame = TRUE, 
       main = "Robbery")
qqline(clearData$Robbery, col = "steelblue", lwd = 2)

qqnorm(clearData$Aggravated_assault, frame = TRUE, 
       main = "Aggravated_assault")
qqline(clearData$Aggravated_assault, col = "steelblue", lwd = 2)

qqnorm(clearData$Property_crime_total, frame = TRUE, 
       main = "Property_crime_total")
qqline(clearData$Property_crime_total, col = "steelblue", lwd = 2)

qqnorm(clearData$Burglary, frame = TRUE, 
       main = "Burglary")
qqline(clearData$Burglary, col = "steelblue", lwd = 2)

qqnorm(clearData$Larceny_theft, frame = TRUE, 
       main = "Larceny_theft")
qqline(clearData$Larceny_theft, col = "steelblue", lwd = 2)

qqnorm(clearData$Motor_vehicle_theft, frame = TRUE, 
       main = "Motor_vehicle_theft")
qqline(clearData$Motor_vehicle_theft, col = "steelblue", lwd = 2)


subData = clearData[,c("Violent_crime_total","Murder_and_Manslaughter", 
                       "Forcible_rape","Robbery","Aggravated_assault",
                       "Property_crime_total", "Burglary", "Larceny_theft", 
                        "Motor_vehicle_theft", "total")]
head(subData)

options(repr.plot.width=30, repr.plot.height=15)
ggpairs(subData) + theme_bw

#4
#Fitting linear regression models
LinearModel <- lm(total ~ .,data=clearData)
summary(LinearModel)

#Evaluation
evaluate = clearData$total
evaluate = ifelse(evaluate >= 9000,"bad","good")
observe = table(evaluate)
View (observe)

 





