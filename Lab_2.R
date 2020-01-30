library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(fastDummies)
#debugonce(fGarch::garchFit)

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")


names(ameslist)

typeof(ameslist)

unique(ameslist$GarageType)

GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )

ameslist <- subset(ameslist, is.na(GarageType)==FALSE)
ameslist <- cbind(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

#Exercise 1:

#====================================1.0===================================

int_type = lapply(ameslist, class)
#get name which type is int 
Ames = ameslist[int_type=='integer']

names(Ames)
#check the names we want to leave
Ames <-Ames[ , !(names(Ames) %in% c("MSSubClass", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmntUnfSF", "LowQualFinSF", "X3SsnPorch", "MiscVal"))]

names(Ames)
#save the new select data
save(Ames, file = "Ames.txt")

#====================================2.0===================================
#12 of the variables that are type = int in the data set.
features <-c("SalePrice","BedroomAbvGr","TotRmsAbvGrd","GarageCars", "YrSold", "GrLivArea", "TotalBsmtSF", "OverallCond","OverallQual","LotArea","YearBuilt","YearRemodAdd")
pairs(Ames[,features])

#====================================3.0===================================
cor(Ames[,features])

#====================================4.0===================================
attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea)
plot(Ames$GrLivArea, Ames$SalePrice, main = "SalePrice with  GrLivArea", ylab = "SalePrice", xlab = "GrLivArea") + abline(lm.fit)

head(Ames)
Scatter_Ames = pairs(Ames[,c(3,4,12,13,14,16,19,20,23,26,32,33)], pch = 19,
                     lower.panel = NULL)

#Exercise 2:


#====================================1.0===================================

ameslist$GarageOutside <- ifelse(ameslist$GarageType_Detchd == 1 | ameslist$GarageType_CarPort == 1, 1, 0)

model1 = lm(SalePrice ~ ameslist$GarageOutside)
summary(model1)

#====================================2.0===================================
model2 <- lm(SalePrice ~ ., data = Ames)
summary(model2)

#====================================3.0===================================
plot(model2)


#====================================4.0===================================
model4 <- lm(SalePrice ~ GrLivArea + OverallQual + LotArea + LotFrontage +  LotFrontage*LotArea, data=Ames)
summary(model4)

#====================================5.0===================================

model5 <- lm(SalePrice ~ log(GrLivArea), data = Ames)
summary(model5)


model6 <- lm(SalePrice ~ LotArea + I(LotArea^2), data = Ames)
summary(model6)

model7 <- lm(SalePrice ~ sqrt(LotArea), data = Ames)
summary(model7)


