library(readxl)
library(ggplot2)

#qestion 4.1
data <- read_excel("tecator.xlsx") #load a data
data <- as.data.frame(data) # coonvert into data frame

ggplot(data, aes( x=data$Protein, y=data$Moisture) ) +
  geom_point() + geom_smooth(method=lm)

#comment 
#As you can see in the plot the data between mositure and protine is not well described by linear model 
#most of the data points are away from the linear model line so data is not linear



#question 4.2
# Moisture ~ N(wo+w1x^1+w2x^2+w3x^3+w4x^4+w5x^5+w6x^6,sigma_square) OR  M <- wo+w1x^1+w2x^2+w3x^3+w4x^4+w5x^5+w6x^6 + epsilon 
# where epsilon ~ N(0,sigma_square)  and x = Protein 

#MSE is criterion is used for selecting best estimator , in gaussion distrubution or normal distirbution 
#which are unbiased esimator so in such distribution it has minimize MSE which is equalivent to Mininizing 
#variance which is always non negative and values are close to Zero are better so thats why we select a MSE 


create_model_and_plot <- function(training, validation)
{
  result <- list()
  training_mse <- c()
  validation_mse <- c()
  for (i in 1:6) 
  {
    #for traning data
    model <- lm(Moisture ~ poly(Protein, degree = i, raw = TRUE) , data = training)
    y_hat <- predict(model, training)
  
    training_mse[i] <- mean((training$Moisture - y_hat)^2 )
    # validation data fiting
    y_hat <- predict(model,  validation)
    validation_mse[i] <- mean((validation$Moisture - y_hat)^2)
  }
  ind <- seq(from=1,to=6,by=1)
  result <- data.frame(training=training_mse, validation=validation_mse, x=ind)
  
}



# 4.3
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,] #train is data 
validation=data[-id,] #test data is newData
result<- create_model_and_plot(train, validation)


ggplot()+ geom_line(aes(x=result$x,y=result$training, color ="training data" )) +
  geom_line(aes(x =result$x, y = result$validation, color = "validation data"))+
  ggtitle("MSE for poly Models")+xlab("upto 6 polynomial Degree") +ylab("Mean Square Error")

#Accourding to plot on which X-axis show the model Numbber with respect to it complexity 
#from 1..6, model with Less MSE value is best so in that case model with drgree of ploynomial 6 
#of traning data has less MSE value so this model is good because it has less MSE value since its 
#has less variance but model has more complexity due to the order of polynomial with high baised 
# Although we cannot explicitly observe the bias or the variance of model

#4.4
library(MASS)
new_data <- data[2:102]
fit <- lm(Fat ~ . , data = new_data)
coef(fit)
step <- stepAIC(fit, direction = "both")
summary(step)
# 63 variables have been selected for the final model which can be shown by anova component of stepwise
#function


#4.5
library(glmnet)
response <- new_data$Fat
predictors <- as.matrix(new_data[1:100])
model10 <- glmnet(predictors ,response, alpha = 0, family = "gaussian"  ) #apha= 0 ridge regression
plot(model10, xvar="lambda", label=TRUE)

#the plot shows the 100 variable impact with increase in lambada, log lambda is on X- axis and cofficients
#are on y-axis with increase in log lambda, the model coefficients converges to 0 that 
# is for higher values of lambda, for the model

#4.6
#aplha 1 for lasso
model10 <- glmnet(predictors ,response, alpha = 1, family = "gaussian"  )
plot(model10, xvar="lambda", label=TRUE)

#comment
#in the graph shows that cofficients converages fast at lambda = 0 , before that lambada > 0
#the cofficients do not converages fast, variable channel41 has large cofficient value so 
#it converges to zeror at lambda = 2
#higher cofficient of lambda decays slowly when lambda > 0

# When the lasso plot is compared to the one for ridge regression, the selection of 
# model coefficients  for ridge regression all coefficiennts are 
# selected while Lasso does not select all coefficients. Moreover for ridge regression,
# the coefficients does not converges to 0 for lambda = 0 as compared to the plot of 
# Lasso

#4.7
lambda.seq <- seq(0,1,0.001) #by default it will not consider
model=cv.glmnet(predictors, response, alpha=1,family="gaussian",lambda= lambda.seq)
model$lambda.min
coef(model, s="lambda.min")
model$lambda.1se
coef(model, s="lambda.1se")
plot(model)
#comment
#After applying Lasso with series of lambda's including Zero the plot indicate the best model with the dotted line and the variable which is selected
# from 100 variable . For lambda = 0 is the optimal lambda value , but for lambda = 0 
#all varaibale has been chosen and variable is shirkage to 0 very fast

#it can be seen in the plot that increase in lambda value increase the MSE value 
#for highest value of lambda which is 1 , MSE becomes more than 100 variables


#4.8
#on step 4 we use stepAic for both ddirection, forward and backward finds the best subest of variables
#which are 38 which fit the best to a model.
#On Step 7 Forlasso regression we use series of lambada by including lambda = 0 is also consider
#it is not able to find the optimal values of lambda 



