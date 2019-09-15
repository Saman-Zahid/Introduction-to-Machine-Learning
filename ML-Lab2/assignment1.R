  #assignment 1.1
  library(ggplot2)
  library(stats)
  library(MASS)
  data<- read.csv2("australian-crabs.csv" ,sep = ",",dec=".")
  p <- ggplot(data, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) + 
    scale_color_manual (values =  c('blue', 'red')) +
    labs(x="CL carspace length", y="RW rear Width", colour="Classes") +
    ggtitle("original data")
    
  p
  X<- data.frame(RW=data$RW , CL=data$CL )
  Y <- data$sex
  
  
  #ASSIGNMENT 1.2
  library(MASS)
  disc_fun=function(label, S)
  {
    X1=X[Y==label,]
    mean_v <- c(mean(X1$RW) ,mean(X1$CL))
    covaiance_mat_inverse <- solve(S)
    prior_prob <- nrow(X1) / nrow(X)
    w1 <- covaiance_mat_inverse %*% mean_v
    b1 <- ((-1/2) %*% t(mean_v) %*% covaiance_mat_inverse  %*% mean_v) + log(prior_prob)
    w1<- as.vector(w1)
    
    return(c(w1[1], w1[2], b1[1,1]))
  }
  
  X1=X[Y=="Male",]
  X2=X[Y=="Female",]
  
  S=cov(X1)*dim(X1)[1]+cov(X2)*dim(X2)[1]
  S=S/dim(X)[1]
  
  #discriminant function coefficients
  res1=disc_fun("Male",S)
  res2=disc_fun("Female",S)
  
  #decision boundary coefficients 'res'
  res <- c( -(res1[1]-res2[1]) , (res2[2]-res1[2]), (res2[3]-res1[3]))

  # classification
  d=res[1]*X[,1]+res[2]*X[,2]+res[3]
  Yfit=(d>0)
  plot(X[,1], X[,2], col=Yfit+1, xlab="CL", ylab="RW")

  #slope and intercept
  slope <- (res[2] / res[1] ) * -1
  intercept <- res[3] /res[1] * -1
  
  #plot decision boundary 
  X<- cbind(X,sex=Y)
  p <- ggplot(X, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) + 
    scale_color_manual (values =  c('blue', 'red')) +
    labs(x="CL carspace length", y="RW rear Width", colour="Classes") +
    geom_abline(slope = slope, intercept = intercept) + ggtitle("Descion Boundary LDA")
  p
  
  
  #1.4
  library(MASS)
  logistic_fit <- glm(sex ~ RW + CL, data= X, family='binomial')
  #lda misclasficafication
  lda_result<- lda(sex ~ RW + CL, data= X)
  y_predict <- predict(lda_result, method = "predictive")
  conf_mat <- table(y_predict$class, X$sex)
  misclassfication <- 1 - sum(diag(conf_mat))/sum(conf_mat)
  
  qda_result<- qda(sex ~ RW + CL, data= X)
  y_predict <- predict(qda_result, method = "predictive")
  conf_mat <- table(y_predict$class, X$sex)
  misclassfication <- 1 - (sum(diag(conf_mat))/sum(conf_mat))
  
  r <- as.numeric(unlist(logistic_fit$coefficients))
  r<-data.frame(intercept=r[1] , RW=r[2], CL=r[3]) #convert data into datafreame
  glm_intercept <- (-r$intercept/r$RW) #+ (0.5/r$RW)
  
  # glm_slope <- (w / r$RW )
  glm_slope <- (-r$CL / r$RW ) #+ (0.5) / r$RW
  p <- ggplot(X, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) + 
    scale_color_manual (values =  c('blue', 'red')) +
    labs(x="CL carspace length", y="RW rear Width", colour="Classes") +
    geom_abline(slope = glm_slope, intercept = glm_intercept) + ggtitle("Logistic Regression decision Boundary")
  p
    