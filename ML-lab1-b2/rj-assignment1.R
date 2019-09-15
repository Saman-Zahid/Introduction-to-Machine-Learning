rm(list = ls())
library(ggplot2)
library(gridExtra)
spambase <- read.csv2("spambase.csv", header = TRUE, sep = ";", quote = "\"",
                      dec = ",", fill = TRUE)
spambase <- as.data.frame(spambase)

library(mboost)
library(randomForest)

n=dim(spambase)[1]
set.seed(12345)
id=sample(1:n, floor(n*2/3))
train=spambase[id,]
test=spambase[-id,]

n_trees <- seq(from = 10,to = 200, by = 10)
adBoost_misclassfication <- c()

for (i in 1:length(n_trees)) 
{
  #missclassfication traning
  fit <- blackboost(as.factor(Spam) ~., data = train,
                    control = boost_control(mstop = n_trees[i], 
                                            nu=0.1),
                    family =  AdaExp())
  ypredict <- predict(fit, newdata = test, type= "class")
  conf_mat <- table(ypredict,test$Spam) # misclassification
  adBoost_misclassfication[i] <- 1 - sum(diag(conf_mat))/sum(conf_mat)
}

#plot test error
plot(adBoost_misclassfication,type = "b",main="ADBOOST Test Missclassfication", 
     xlab= "Number of Trees", 
     ylab= "missclassfication rate",
     col="red", pch=19, cex=1)

# Loss Function = e(y-f)^2

rf_misclassfication <- c()
training = sample(1:n,floor(n*2/3))
for (i in 1:length(n_trees)) 
{
  #missclassfication test
  fit <- randomForest(as.factor(Spam) ~ ., data=train,
                      # subset = training,
                      importance=TRUE,
                      ntree = n_trees[i])
  ypredict <- predict(fit, newdata = test,type ="class") #missclassfication
  conf_mat <- table(ypredict,test$Spam)
  rf_misclassfication[i] <- 1 - sum(diag(conf_mat))/sum(conf_mat)
  
}

#plot error random forest
plot(rf_misclassfication,type = "b",main="Random Forest Test Missclassfication", 
     xlab= "Number of Trees", 
     ylab= "Misclassfication rate",
     col="red", pch=19, cex=1)



# comparsion Between graph

plot(y = adBoost_misclassfication,x=n_trees, type = "l", col="red", 
     main= "Performance Evaluation of Adaboost Vs Random Forest", 
     xlab = "Number of Trees",
     ylab="Misclassification Rate", 
     ylim = c(0,0.15))

points(y = adBoost_misclassfication,
       x=n_trees,col="red", 
       pch=19, cex=1)

lines(y = rf_misclassfication,
      x=n_trees, 
      type= "l", col = "blue")

points(y = rf_misclassfication,x=n_trees,
       col="blue", pch=19, 
       cex=1)

legend("topright",legend= c("adaboost","random forest"),
       col=c("red","blue"),lty=1,cex=0.8)

