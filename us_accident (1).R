library(MASS)
library(readr)
library(rpart)
library(kknn)
library(caret)
library(dplyr)
library(knitr)
setwd('~/Desktop/US_ACCIDENT')
train <- readRDS('train_.rds')
dim(train)#49 vars
train1 = train[sample(nrow(train),15000),]
#1.Data preprocessing
#1.delete variables with 0 variance
train1 = train1[,!nearZeroVar(train1,saveMetrics = T)$nzv]
ncol(train1)#35 
#2. delete insignificant vars
xxnonsignificant <- c('ID','Source','TMC','Description','City',
                      'Country','Street')
train2 <- train1[,!names(train1) %in% xxnonsignificant]
#The summary of the dataset is shown below:
summary(train2)
#2.turn into factor
train2$Timezone = as.factor(train2$Timezone)
train2$Wind_Direction = as.factor(train2$Wind_Direction)
train2$Weather_Condition = as.factor(train2$Weather_Condition)
train2$Civil_Twilight = as.factor(train2$Civil_Twilight)
train2$Sunrise_Sunset = as.factor(train2$Sunrise_Sunset)
train2$Nautical_Twilight = as.factor(train2$Nautical_Twilight)
train2$Astronomical_Twilight=as.factor(train2$Astronomical_Twilight)


#missing num
sum(is.na(train2))
train2.na <- train2[!complete.cases(train2),] 
head(train2.na,2)

train$Severity <- as.factor(train$Severity)
v1 <- ggplot(train2,aes(Severity))+geom_bar(position="identity",fill="#FFA500")
v1

v2 <- train2 %>% group_by(Severity,State) %>% count() %>% arrange(desc(n))
ggplot(v2, aes(fill=Severity,y=n, x=State))+ geom_bar(stat = "identity")

v3 <- train2 %>% group_by(Severity,Sunrise_Sunset) %>% count() %>% arrange(desc(n))
ggplot(v3, aes(fill=Severity,y=n, x=Sunrise_Sunset))+ geom_bar(position="dodge",stat = "identity")
v4 <- train2 %>% group_by(Severity,Nautical_Twilight) %>% count() %>% arrange(desc(n))
ggplot(v4, aes(fill=Severity,y=n, x=Nautical_Twilight))+ geom_bar(position="dodge",stat = "identity")


v5 <- train2 %>% group_by(Severity,Traffic_Signal) %>% count() %>% arrange(desc(n))
ggplot(v5, aes(fill=Severity,y=n, x=Traffic_Signal))+ geom_bar(position="dodge",stat = "identity")+theme()


library(lubridate)
train2$Start_Time <- as_datetime(train2$Start_Time)
train2$End_Time <- as_datetime(train2$End_Time)
train2$accident_time <- round(abs((train2$Start_Time-train2$End_Time)/60))
train2$Year <- as.numeric(format(train2$Start_Time,format="%Y"))
train2$month <- as.numeric(format(train2$Start_Time,format="%m"))
train2$date <- as.numeric(format(train2$Start_Time,format="%d"))
data_time <- train2 %>% dplyr::select(Year) %>% group_by(Year) %>% summarise(num=n()) %>% arrange(num)
head(data_time)
ggplot(data_time,aes(x=Year, y=num)) + geom_bar(stat="identity", fill = "#6495ED")+
  ggtitle('2016-2019 numbers of accidents')+ theme_grey(base_family = "STKaiti")
accurate_data_time_2018 <- train2 %>% group_by(month,Year) %>% filter(Year == 2018)%>%summarise(total.count=n())
# Plot
accurate_data_time_2018 %>%
  ggplot( aes(x=month, y=total.count)) +
  geom_line() +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  ggtitle("2018-nums of accident")

# Model(RF & KNN)
### 1. deal with the data
set.seed(1)
train2$County=as.factor(train2$County)
kernel.type=c("rectangular","triangular","epanechnikov","biweight","triweight","cos","inv","gaussian","rank","optimal")
model_var = c('Severity','Start_Lat','Start_Lng','Distance(mi)','Side',
              'Temperature(F)','Humidity(%)','Pressure(in)','Wind_Direction',
              'Wind_Speed(mph)','Precipitation(in)','Crossing','Junction',
              'Traffic_Signal','Sunrise_Sunset','accident_time','Year','month','date')
model_data = train2[,names(train2) %in% model_var]
model_data$Severity = model_data$Severity %>% factor()
model_data$Side = model_data$Side %>% factor()
summary(model_data)
model_data = model_data[complete.cases(model_data),]
colnames(model_data)=c('Severity','Start_Lat','Start_Lng','Distance','Side',
                       'Temperature','Humidity','Pressure','Wind_Direction',
                       'Wind_Speed','Precipitation','Crossing','Junction',
                       'Traffic_Signal','Sunrise_Sunset','accident_time','Year','month','date')

## Data modeling
model.tkknn <- train.kknn(Severity~.,model_data,kmax=48,kernel = kernel.type,distance=2,scale=T)
plot(model.tkknn)
model.tkknn #print out the best parameters
#predict with the best parameter
set.seed(123)
testx <- model_data[,-1]
train_data = model_data[sample(nrow(model_data),nrow(model_data)/3),]
test_data = model_data[-sample(nrow(model_data),nrow(model_data)/3),]
#split train and test dataset
model.kknn <- kknn(Severity~.,train_data,test_data,k=model.tkknn$best.parameters$k,scale=T,distance=2,kernel=model.tkknn$best.parameters$kernel)
confusionMatrix(model.kknn$fitted.values,test_data$Severity)# Accuracy : 0.7347

#randomforest
library(randomForest)
train_data$Severity=factor(train_data$Severity)
rf.model <- randomForest(Severity~., data=train_data)
rf.model
summary(train_data)
pred.rf = predict(rf.model,test_data)
confusionMatrix(pred.rf,test_data$Severity)
varimp <- varImp(rf.model) %>% as.data.frame() 
varimp <- cbind(varimp,rownames(varimp)) 
varimp <- varimp %>% arrange(desc(Overall))
varimp

