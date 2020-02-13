

#function to get rmse
rmse = function(actual, predicted) {
  i.good = !is.na(predicted)
  return(sqrt(mean((actual[i.good] - predicted[i.good]) ^ 2)))
}


######### Exercise 1 ############
#1
Ames = read.csv("ames.csv")
Ames = subset(Ames, select=-c(OverallCond, OverallQual))

summary(Ames)
#I will also remove Alley (1369 NAs), PoolQC (1453 NAs), Fence (1179 NAs), MiscFeature (1406 NAs)
Ames = subset(Ames, select=-c(Alley, PoolQC, Fence, MiscFeature))


#2
name.predictor = names(Ames)[-ncol(Ames)]
Model = list()  #to store the 15 models
RMSE = rep(0, 15) #vector to store rmse
for(i in 1:15){      #number of predictors in the model
  rmse.each = rep(0, length(name.predictor)) #to store rmse with each addition
  for(j in 1:length(name.predictor)){  #number of reminding variables
    if(i==1){      #set up the first variable
      formula0 = name.predictor[j]
      formula1 = paste0("SalePrice~", formula0)
    }else{formula1 = paste0("SalePrice~", formula0, "+", name.predictor[j])}
    
    m0 = lm(as.formula(formula1), data=Ames) #linear model
    pred = predict(m0)         #prediction
    
    if(length(pred) == 1460){
      rmse.each[j] = rmse(Ames$SalePrice, pred)
    }else{rmse.each[j] = rmse(Ames$SalePrice[-m0$na.action], pred)}
  }
  i.best = which.min(rmse.each)  #the index of the best variable
  if(i==1){
    formula0 = name.predictor[i.best]
  }else{formula0 = paste0(formula0, "+", name.predictor[i.best])}
  name.predictor = name.predictor[-i.best]  #update variable names
  
  model = lm(as.formula(paste0("SalePrice~", formula0)), data=Ames)
  pred = predict(model)
  if(length(pred) == 1460){
    RMSE[i] = rmse(Ames$SalePrice, pred)
  }else{RMSE[i] = rmse(Ames$SalePrice[-model$na.action], pred)}
  
  Model[[i]] = model
}


#3
complexity = 1:15
plot(RMSE ~ complexity, type="b")



######### Exercise 2 ############
get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

#1
set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]
test_data = test_data[test_data$Exterior1st!="AsphShn",] #it seems AsphShn does not appear in the model

train_rmse = sapply(Model, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(Model, get_rmse, data = test_data, response = "SalePrice")

plot(1:15, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(1:15, test_rmse, type = "b", col = "darkorange")
legend("topright", c("train", "test"), pch=1, lty="solid", col=c("dodgerblue", "darkorange"))

#2
#### data clean  #####
#remove variables which have many NAs
Ames0 = subset(Ames, select = -c(Id, LotFrontage, FireplaceQu))

#remove categorical variables which is extremely uneven distributed
Ames0 = subset(Ames0, select = -c(Street, Utilities,  Condition2, 
                                 RoofMatl, Heating))

#remove continous variables which contain many 0
n.zeros =  rep(0, 67)
names(n.zeros) = names(Ames0)
for(i in 1:67){n.zeros[i] = sum(Ames0[,i]==0, na.rm=T)}
n.zeros
Ames0 = subset(Ames0, select = -c(X3SsnPorch, PoolArea, MiscVal))

#remove NAs
Ames0 = Ames0[complete.cases(Ames0), ]


######## stepwise ########
name.predictor = names(Ames0)[-ncol(Ames0)]
Model0 = list()  #to store the 15 models
RMSE0 = rep(0, 10) #vector to store rmse
for(i in 1:10){      #number of predictors in the model
  formula1 = paste0("SalePrice~", paste(name.predictor, collapse="+"))
  m0 = lm(as.formula(formula1), data=Ames0) #linear model
  z = anova(m0)
  i.largest = which.max(z$`Pr(>F)`)
  name.predictor = name.predictor[-i.largest]  #update variable names
  
  model = lm(as.formula(paste0("SalePrice~", paste(name.predictor, collapse="+"))), data=Ames)
  pred = predict(model)
  RMSE0[i] = rmse(Ames0$SalePrice, pred)
  
  Model0[[i]] = model
}

plot(63:54, RMSE0, type="b", xlab="complexity")



#split dataset
set.seed(9)
num_obs = nrow(Ames0)
train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data0 = Ames0[train_index, ]
test_data0 = Ames0[-train_index, ]

model.final = Model0[[2]]
rmse.train = get_rmse(model.final, data = train_data0, response = "SalePrice")
rmse.test = get_rmse(model.final, data = test_data0, response = "SalePrice")


#3
summary(Model0[[2]])
anova(Model0[[2]])

#4
plot(Ames0$SalePrice, model.final$fitted.values, xlab="real value", ylab="predicted value")
abline(a=0,b=1, col="red")
hist(model.final$residuals, 20)


