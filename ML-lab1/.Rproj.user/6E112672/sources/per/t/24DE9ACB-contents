library(ggplot2)
library(kknn)
knearest=function(data,k,newdata) 
{
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X = as.matrix(data[,-p])
  Y = as.matrix(newdata[-p]) # change xn to Yn
  X_hat = X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Y_hat = Y/matrix(sqrt(rowSums(Y^2)), nrow = n2 , ncol = p - 1)
  C <- X_hat %*% t(Y_hat)
  D <- 1 - C #distacne matrix calculate
  for (i in 1:n2 )
  {
    Ni <- order(D[,i])
    N_i <- data[Ni[1:k],"Spam"]  # get k values
    Prob[i] <- sum(N_i) / k
  }
  return(Prob) #return proabilities
}

##########################load excel file and divide data #######################
library(readxl)
data <- read_excel("spambase.xlsx")
data <- as.data.frame(data) # coonvert into data frame

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,] #train is data 
test=data[-id,] #test data is newData
####################################################################

##### step 3 
probalities <- knearest(train,5, test)

new_probalities <- ifelse(probalities > 0.5, 1,0)
conf_mat <- table(spam = test[,ncol(data)] , predicted_val = new_probalities)

miss_classfication_step3 <-  1 - sum(diag(conf_mat))/sum(conf_mat) #missclassfication

##### step 4
probalities <- knearest(train,1, test) #repeat step 3 for K = 1 which is 

probalities <- ifelse(probalities > 0.5, 1,0)
conf_mat <- table(spam = test[,ncol(data)] , predicted_val = probalities)
miss_classfication_step4 <- 1 - sum(diag(conf_mat))/sum(conf_mat)

#### step 5
knn <- kknn(Spam ~. , train, test , k = 5 ) #standard kknn method at  K = 5
probalities <- knn$fitted.values
probalities <- ifelse(probalities > 0.5, 1,0)

conf_mat <- table(spam = test[,ncol(data)] , predicted_val = probalities)

miss_classfication_step5 <- 1 - sum(diag(conf_mat))/sum(conf_mat) #missclassfication

############## standard kknn for K = 1
knn <- kknn(Spam ~. , train, test , k = 1 )  #standard kknn method at  K = 5
probalities <- knn$fitted.values
probalities <- ifelse(probalities > 0.5, 1,0)

conf_mat <- table(spam = test[,ncol(data)] , predicted_val = probalities)
miss_classfication <- 1 - sum(diag(conf_mat))/sum(conf_mat) / sum(conf_mat) #missclassfication


sprintf("knearest at k = 5 misclassfication value = %f ", miss_classfication_step3)
sprintf("standard kknn at k = 5 misclassfication value = %f ", miss_classfication_step5)

sprintf("knearest at k = 1 misclassfication value = %f ", miss_classfication_step4)
sprintf("standard kknn at k = 1 misclassfication value = %f ", miss_classfication)

#### step 6 

ROC <- function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m)
  {
    t <- table(Y,Yfit>p[i])
    TPR[i] <-  t[2,2]/sum(t[2,])
    FPR[i] <-  t[1,2]/sum(t[1,])
  }
  return (list(TPR=TPR,FPR=FPR))
}


pi_values <- seq(from = 0.05, to= 0.95 , by=0.05)
Y <- train[,ncol(data)]
kkn <-  kknn(Spam ~., train , test , k = 5) #built in Knn for k = 5
knearest_p <- knearest(train, 5 , test) #knearst k = 5
kkn_p <- kkn$fitted.values # knn proabilties

#debugonce(ROC)
roc_curve_knearest <- ROC(Y, knearest_p , pi_values)
roc_curve_kkn_p <- ROC(Y, kkn_p , pi_values)

#plot graoh
X<-  as.data.frame(roc_curve_knearest)
Y <- as.data.frame(roc_curve_kkn_p)
ggplot() + 
  geom_line(data = X, aes(x = X$FPR, y = X$TPR), color = "red") +
  geom_line(data = Y, aes(x = Y$FPR, y = Y$TPR), color = "blue")+
  ggtitle("ROC curve using kknn() & Knearnest()")+xlab("FPR") +ylab("TPR")

sensitivity_kn <- 1 - roc_curve_knearest$FPR
sensitivity_knn <- 1 - roc_curve_kkn_p$FPR




a<-data.frame("TRUE"=c("TP","FP"), "FALSE"= c("FN","TN"), "total"=c("N+","N-"))
row.names(a) <- c(TRUE,FALSE)
a
