library(neuralnet)
library(tidyverse)

# Q1(a) Using asg_6_train.csv, train a feedforward neural network with 2 hidden layers, where each hidden
# layer has 2 units, using the function neuralnet() from the package neuralnet.
asg6_train <- read_csv("D:/STAT 362/asg_6_train.csv")

asg6_test <- read_csv("D:/STAT 362/asg_6_test.csv")

normalize  <- function(train, test) {
  train_n <- train
  test_n <- test
  
  train_min <- apply(train, 2, min)
  train_max <- apply(train, 2, max)
  
  for (i in 1:ncol(train)) {
    train_n[, i] <- (train[, i] - train_min[i]) / (train_max[i] - train_min[i])
    
    test_n[, i] <- (test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  }  
  
  return(list(train = train_n, test = test_n))
}

asg6_train_n <- normalize(asg6_train, asg6_test)$train


set.seed(1)

asg6_ANN <- neuralnet(y~., data = asg6_train_n, hidden = c(2,2))




# Q1(b) Using asg_6_test.csv, evaluate the performance of the model in (a).
asg6_test_n <- normalize(asg6_train, asg6_test)$test

prediction <- neuralnet::compute(asg6_ANN, asg6_test_n[, 2:4])

predicted_y <- prediction$net.result[, 1]

cor(asg6_test_n$y, predicted_y)
## [1] 0.770712




# Q1(c) Using asg_6_train.csv, fit a linear regression model.
fit <- lm(y~., data = asg6_train)
summary(fit)
## 
## Call:
##   lm(formula = y ~ ., data = asg6_train)
## 
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.5593 -0.3397 -0.0622  0.2893  3.0443 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.215e-16  6.338e-02   0.000   1.0000    
## x1          4.148e-01  6.370e-02   6.513 6.07e-10 ***
##   x2          1.955e-01  6.357e-02   3.076   0.0024 ** 
##   x3          2.604e-02  6.370e-02   0.409   0.6831    
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8963 on 196 degrees of freedom
## Multiple R-squared:  0.2088,	Adjusted R-squared:  0.1967 
## F-statistic: 17.24 on 3 and 196 DF,  p-value: 5.642e-10





# Q1(d) Using asg_6_test.csv, evaluate the performance of the model in (c).
prediction_y2 <- predict(fit, asg6_test)

cor(asg6_test$y, prediction_y2)
## [1] 0.2438606




# Q1(e)
# The feedforward neural network method has a higher value of correlation which
# means that it has a better prediction and therefore it has a better performance.
