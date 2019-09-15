data <- read.csv2("data.csv", header = TRUE, sep = ";", quote = "\"",
                  dec = ",", fill = TRUE, check.names = FALSE)
data1 <- as.data.frame(data)
data_email <- as.data.frame(data)
data_email$Conference <- as.factor(data$Conference)

n=dim(data_email)[1]
set.seed(12345)
# 70% Training Data
id=sample(1:n, floor(n*0.7))
train=data_email[id,]
# 30% validation & testing Data
test = data_email[-id,]


library(pamr)

rownames(train) <- 1:nrow(train)

which(colnames(train)=="Conference")

x <- t(train[,-4703])
y <- train[[4703]]

#test_x <- t(test[,-4703])

mydata <- list(x=x, y= as.factor(y), geneid=as.character(1:nrow(x)),genenames = rownames(x))

model_train <- pamr.train(mydata)

cv_model <- pamr.cv(model_train, data = mydata)

# threshold for which errors are least is 1.306 with 5 errors

pamr.plotcv(cv_model) # legend is not shown in plot and only 2 lines are drawn why?
print(cv_model)

model_fit <- pamr.train(mydata, threshold = cv_model$threshold[which.min(cv_model$error)])

# par(mfrow=c(1,1),mar=c(2,2,2,2))
pamr.plotcen(model_train, mydata, threshold = cv_model$threshold[which.min(cv_model$error)])
## interpretation?

features = pamr.listgenes(model_train, mydata, threshold = 1.306, genenames=TRUE)
cat( paste( colnames(train)[as.numeric(features[1:10,1])], collapse='\n' ) )

# The above features are the most contributing based on the score which greatly 
# discriminates conference announcements from other emails since the score difference between 
# the two classes is highest for these features

# 
# ypredict <- pamr.predict(model_train, newx = test_x, type = "class", threshold = 1.306)
# 
# conf_mat <- table(ypredict, test$Conference)
# misc <- 1 - (sum(diag(conf_mat))/sum(conf_mat)) 

misclas <- pamr.confusion(cv_model, threshold = 1.306)

## The overall error is calculated to be 0.136 which is indicates a good fit


### Question 2

library(glmnet)
set.seed(12345)
response <- train$Conference
predictors <- as.matrix(train[,-4703])

elastic_model <- glmnet(x=predictors,y=response,family = "binomial",alpha = 0.5)


cv.fit <- cv.glmnet(x=predictors,y=response,family="binomial",alpha = 0.5)
cv.fit$lambda.min

par(mar=c(3,3,3,3))
plot(cv.fit)
plot(elastic_model)

predictor_test <- as.matrix(test[,-4703])
#predict.glm
ypredict <- predict(object = elastic_model,newx = predictor_test, s = cv.fit$lambda.min,
                    type = "class", exact = TRUE)


selected_coeff<- as.numeric(coef(elastic_model, s=cv.fit$lambda.min))
number_of_feature <- length(selected_coeff[selected_coeff != 0])

confusion_mat <- table(ypredict,test$Conference)

misclassification <- 1 - (sum(diag(confusion_mat))/sum(confusion_mat))


library(kernlab)

x <- as.matrix(train[,-4703])
y <- train[,4703]

svm_fit <- ksvm(data = train,Conference ~ . ,kernel="vanilladot", 
                scaled = FALSE)

ypred <- predict(svm_fit, newdata = test, type="response")

confusion_mat <- table(ypred,test$Conference)

misclas_svm <- 1 - sum (diag(confusion_mat))/sum(confusion_mat)


## draw comparative table

## Question 3

pvals <- c()


for (i in 1:(ncol(data1)-1)) {
  
  ttest <- t.test(data1[,i] ~ Conference, data = data1, alternative = "two.sided")
  pvals[i] <- ttest$p.value
}

pvalues_df <- data.frame(p_value=pvals,feature=1:(ncol(data1)-1) ) 
pvalues_df <- pvalues_df[order(pvalues_df$p_value),]


ALPHA <- 0.05
L <- c()
it <- 1
# Let alpha = 0.05  ## ask oleg
for (j in 1:nrow(pvalues_df)) {
  if(pvalues_df$p_value[j] < ALPHA * (j / nrow(pvalues_df)) )
  {
    L[it] <- j
    it <- it +1
  }
}
max(L)

LL = pvalues_df$p_value[max(L)]
LL
newPvalues <- c()
pvalue_feature <- c()
pvalue_status <- c()
j<- 1
for (j in 1:nrow(pvalues_df)) 
{
  
  pvalue_status[j] <- TRUE
  newPvalues[j] <- pvalues_df$p_value[j]
  pvalue_feature[j] <- pvalues_df$feature[j]
  if(pvalues_df$p_value[j] <= LL)
  {
    pvalue_status[j] <- FALSE
  }
  
}
result <- data.frame(p_value=newPvalues, feature=pvalue_feature,status=pvalue_status)
result

rejected_features <- c()
k <- 1
for (j in 1:(ncol(data1)-1))  {
  
  if(result$status[j] == FALSE)
  {
    rejected_features[k] <- colnames(data1[result$feature[j]])
    k<- k + 1
  }
}
cat(paste(rejected_features, collapse = '\n'))
ggplot(data = result, aes(x = 1:4702,y=result$p_value, col = status)) + geom_point() + labs(x="feature",y="p-value")

## The features that corresponds to the rejected hypothesis are the 
# ones which are most relevant to "announcement of conference", that 
# is which contributes the most for the classification of announcement of conference 
