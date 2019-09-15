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

#wi <- seq(1,100,1)

number_of_trees <- seq(from = 10,to = 100, by = 10)

adaboost <- function(ntrees)
{
fit <- blackboost(as.factor(Spam) ~., data = train,
           control = boost_control(mstop = ntrees, nu=0.1),
           family = AdaExp())
  

# misclassification test

ypredict <- predict(fit, newdata = test, type= "class")

conf_mat <- table(ypredict,test$Spam)

error_test <- 1-sum(diag(conf_mat))/sum(conf_mat)

}

error_rates_ada <- sapply(number_of_trees, adaboost)


plot(error_rates_ada,type = "b",main="Test Misclassification", xlab= "Number of Trees", ylab= "Error",
     col="blue", pch=19, cex=1)

# Loss Function = exp(-y * f)

training = sample(1:n,floor(n*2/3))

random_forest <- function(ntrees)
{
  fit <- randomForest(as.factor(Spam) ~ ., data=train, importance=TRUE,
                      ntree = ntrees)
  
  # test misclassification
  ypredict <- predict(fit, test,type ="class")
  
  conf_mat <- table(ypredict,test$Spam)
  
  error_test <- 1-sum(diag(conf_mat))/sum(conf_mat)
}

error_rates_random <- sapply(number_of_trees, random_forest)

plot(error_rates_random,type = "b",main="Test Misclassification", xlab= "Number of Trees", ylab= "Error",
     col="blue", pch=19, cex=1)


plot(y = error_rates_ada,x=number_of_trees, type = "l", col="red", 
     main= "Performance Evaluation of Adaboost Vs Random Forest", 
     xlab = "Number of Trees",ylab="Misclassification Rate", ylim = c(0,0.15))
points(y = error_rates_ada,x=number_of_trees,col="red", pch=19, cex=1)
lines(y = error_rates_random,x=number_of_trees, type= "l", col = "blue")
points(y = error_rates_random,x=number_of_trees,col="blue", pch=19, cex=1)
legend("topright",legend= c("adaboost","random forest"),
       col=c("red","blue"),lty=1,cex=0.8)
