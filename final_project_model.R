library(boot)
set.seed(123)
bank <- read.csv("bank-full.csv", header = TRUE, sep = ";")

xfactors <- model.matrix(y~job+marital+education+default+housing+loan+contact+day+month+poutcome,data=bank)[,-1]
x <- as.matrix(data.frame(bank$age,bank$balance,bank$duration,bank$campaign,bank$pdays,bank$previous,xfactors))


bank1 <- data.frame(x, y = bank$y)

# logistic regression
model <- glm(y~., data = bank1, family = 'binomial')
ynew <- predict(model, as.data.frame(x), type = "response")
for(i in 1:length(ynew)){
  if(ynew[i]<0.5){
    ynew[i] <- 0
  } else {
    ynew[i] <- 1
  }
}
result1 <- table(bank$y,ynew)
result1
print(xtable(result1), caption = "comparison between logistic regression prediction results and actual results", comment = FALSE)
pred.rate1 <- (result1[1,1]+result1[2,2])/sum(result1)
pred.rate1
# cross-validation estimate of prediction error
# loss function 
brier <- function(y, pHat) {
  mean((y-pHat)^2)
}
cv.glm(bank1, model, cost = brier, K = 10)$delta
coef1 <- summary(model)$coefficients
print(xtable(coef1, digits = 3, caption = "coefficient estimates for logistic regression model"),comment = FALSE, size ="\\small")

# LASSO
library(glmnet)
model2 <-cv.glmnet(x = x, y = bank$y, alpha = 1, family = 'binomial', type.measure = "mse")
min(model2$cvm)
log(model2$lambda.1se)

ynew2 <- predict(model2, x, type = 'response')
for(i in 1:length(ynew2)){
  if(ynew2[i]<0.5){
    ynew2[i] <- 0
  } else {
    ynew2[i] <- 1
  }
}
coef(model2)
plot(model2$glmnet.fit, "norm", label = TRUE)
plot(model2$glmnet.fit,"lambda", label = TRUE)
result2 <- table(bank$y,ynew2)
result2
print(xtable(result2), caption = "comparison between LASSO prediction results and actual results", comment = FALSE)

pred.rate2 <- (result2[1,1]+result2[2,2])/sum(result2)
pred.rate2
coef2 <- data.frame(coef(model2)[,1])
colnames(coef2) <- "estimate"
coef2[coef2==0] <- NA
print(xtable(coef2, digits = 3, caption = "coefficient estimates for logistic regression model"),comment =FALSE, size ="\\small", na.string = "-")

# SVM
library(e1071)
model3 <- svm(y~., data = bank1)
result3 <- table(bank$y,model3$fitted)
result3
print(xtable(result3), caption = "comparison between SVM prediction results and actual results", comment = FALSE)
pred.rate3 <- (result3[1,1]+result3[2,2])/sum(result3)
pred.rate3


