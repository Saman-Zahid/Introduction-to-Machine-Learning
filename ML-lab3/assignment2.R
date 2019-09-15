library(neuralnet)
library(ggplot2)

set.seed(1234567890)

Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var)) #
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
va_MSE <- c()
tr_MSE <- c()

winit <- runif(31, -1, 1) 
for(i in 1:10) {
  nn <- neuralnet(formula = Sin~Var ,data = tr ,hidden = c(10),
                  threshold = i/1000, startweights= winit)
  va_predict <- compute(nn,va$Var)
  tr_predict <- compute(nn,tr$Var) 
  va_MSE[i] <- sum( (va$Sin  - va_predict$net.result[,1])^2)/ nrow(tr)
  tr_MSE[i] <- sum( (va$Sin  - tr_predict$net.result[,1])^2)/ nrow(tr)
  
}
#comment 
#by visualizing the graph the thresshold value is 4/1000 which the stoping creteria 
#so NN model for such a thresshold is below

#thresshold for stoping NN

stoping_thresshold <- which.min(va_MSE)/1000
nn <- neuralnet(formula = Sin~Var ,data = tr ,hidden = c(10), 
                threshold = stoping_thresshold, 
                startweights= winit)
plot(nn, rep="best" , main="Neural Network Plot")

#another plot
plot(prediction(nn)$rep1, pch=1)
points(trva, col = "red")

#MSE plot
result <- data.frame(MSE=va_MSE, x=c(1:10))
ggplot(result, aes(x=x, y=MSE, color = "red")) +
  geom_point(shape = 16, size = 2, show.legend = FALSE) +
  theme_minimal() + ggtitle("MSE plot For NN") + xlab("Index i") + 
  ylab("MSE Error Rate") 
