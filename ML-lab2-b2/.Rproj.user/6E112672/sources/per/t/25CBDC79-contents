library(readxl)
library(ggplot2)
#library(grid)
#library(gridExtra)
library(reshape2)
data <- read_excel("influenza.xlsx")
data_inf <- as.data.frame(data)

df_plot <- data.frame(time= data_inf$Time, mortality = data_inf$Mortality, influenza = data_inf$Influenza)

df <- melt(df_plot,measure.vars = c("mortality","influenza"))
#  p1 <- ggplot(data_inf, aes(x= Time, y = Mortality)) +
#    geom_point(colour="blue")+
#    geom_point(aes(x= Time, y= Influenza),colour="red")
# p1

#  p1 <- ggplot(data_inf, aes(x= Time, y = Mortality)) +
#    geom_point(colour="blue")+
#
# p2 <- ggplot(data_inf, aes(x= Time, y = Influenza)) + 
#   geom_point(colour="red")

ggplot(df,aes(x= time, y = value,colour = variable)) + geom_point()

#grid.arrange(p1,p2,ncol=1,nrow=2)


## From plot it can be observed that influenza cases have the peaks at the same 
# points where the morltality plot has peaks,
# it shows that when there is increasing rate of mortality, the influenza cases have increased 
# or it can be interpreted as increasing number of influenza cases have affected the mortality rate 

## part 2
library(mgcv)

w <- unique(data_inf$Week)
fit_week <- gam(data_inf$Mortality ~ data_inf$Year + s(data_inf$Week,k=52), data = data_inf,family = "gaussian", method = "GCV.Cp")

pred <- predict(fit_week)

# Probilistic model: y = wo + w1x1 + w2x1^2 + e  (where wo=intercept,  w1= est value of 1st var, w2= est value of 2nd var)

# Probilistic model: $y = -680.589 + 1.233*x1 + s(Week) + {\epsilon}~N(0,{\sigma}^2)   (for w1 confirm with oleg)
#y = N(wo + w1x1 + w2x1^2 + e,sigma^2)
# Probilistic model: $y = N(-680.589 + 1.233*x1 + s(Week) + {\epsilon}) where {\epsilon}~N(0,{\sigma}^2)

## part3 

  pred <- predict(fit_week)
 df_plot <- data.frame(time= data_inf$Time, mortality = data_inf$Mortality, pred = as.vector(pred))
# 
p1 <- ggplot(df_plot, aes(x= time, y = mortality)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,pred),colour = "red")+ coord_cartesian(xlim = c(1995,2003))
p1

## quality of fit??

## identify the trend ????

plot(fit_week)

## the mortality rate seems to decrease in the beginning and has the lowest values 
# between week 20 to 30 and then mortality again rises from week 40 onwards

summary(fit_week) # to check significant values using p values

# since year and week both gives p-values less tan the significance level 
# 0.05 therefore both are significant 

# part 4

fit1 <- gam(data_inf$Mortality ~ Year + s(data_inf$Week,k=52,sp=as.numeric(t(fit_week$sp))), 
            data = data_inf,family = "gaussian")
summary(fit1)
pred1 <- predict(fit1)

fit2 <- gam(data_inf$Mortality ~ Year + s(data_inf$Week,k=52,sp=0.000000001), data = data_inf,family = "gaussian")
summary(fit2)
pred2 <- predict(fit2)


fit3 <- gam(data_inf$Mortality ~ Year + s(data_inf$Week,k=52,sp=1.5), data = data_inf,family = "gaussian")
summary(fit3)
pred3 <- predict(fit3)

df_plot <- data.frame(time= data_inf$Time, mortality = data_inf$Mortality, pred1 = as.vector(pred2),
                      pred2 = as.vector(pred3))
# 
p1 <- ggplot(df_plot, aes(x= time, y = mortality)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,pred1),colour = "red")+
  geom_line(aes(time,pred2),colour = "green")

p1

## the very high annd very low values of penalty factor leads to underfitting of model, 
# as it is evident from the plot in which green line represents the predicted values when 
# penalty factor is too high while red line represents the mortality when penalty factor is too low  
## deviance and degrees of freedom decreases with increase in penalty factor

# part 5

df_plot <- data.frame(time= data_inf$Time, influenza = data_inf$Influenza, residual = as.vector(fit_week$residuals))
# 
p2 <- ggplot(df_plot, aes(x= time, y = influenza)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,residual),colour = "red")

p2


## Yes, it is evident from the plot that the temporal pattern in residuals seems to be correlated 
# to the outbreak of influenza since since the peaks in influenza occurs relative to the peaks in 
# residuals'


## part 6

w <- unique(data_inf$Week)
y <- unique(data_inf$Year)
i <- unique(data_inf$Influenza)

fit_f <- gam(data_inf$Mortality ~ s(data_inf$Year,k=length(y)) + s(data_inf$Week,k=length(w)) 
                                  + s(data_inf$Influenza,k=length(i)), data = data_inf,
                                  family = "gaussian", method = "GCV.Cp")
pred_f <- predict(fit_f)

par(mfrow=c(2,2))
plot(fit_f)

## It can be illustrated from the plots of spline components that mortality does not depend much on
# year and have little change annually that is with weeks, but the mortality shows a significant 
# relation with influenza that is with increasing cases of influenza, mortality increases

par(mfrow=c(1,1))

df_plot <- data.frame(time= data_inf$Time, mortality = data_inf$Mortality, 
                      predicted = as.vector(pred_f))
# 
p3 <- ggplot(df_plot, aes(x= time, y = mortality)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,predicted),colour = "red")

p3

## The plot of original and fitted values implies that this model is better than the previous 
# models as it gives the predicted values closest to the original values. This also indicates that 
# including influenza in modelling has a significant impact on fitting.