bank <- read_csv("bank.csv")

testmodel<-lm(bank$balance ~ bank$age + bank$job + bank$marital + bank$education + bank$default + bank$housing + bank$loan + bank$contact + bank$day + bank$month + bank$duration + bank$campaign + bank$previous + bank$y)
summary(testmodel)
plot(testmodel, main="Test Model")

#Some of the coeficients in my test model aren't jointly related to banks balance. Using the F test I grabbed the #values from the summary of my test model that were statistically significant 

adjustedmodel<- lm(bank$balance ~ bank$age + bank$marital + bank$default + bank$loan)
summary(adjustedmodel)
plot(adjustedmodel, main = "Adjusted Model")

#My adjusted model based on the variables we could not reject now only include how an individuals age, marital
#status, default history, and loan history effect thier bank balance.