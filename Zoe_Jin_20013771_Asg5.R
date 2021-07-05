library(class)
library(tidyverse)
library(C50)

# Q1
iris_train <- read.csv("C:/Users/ROG/Desktop/iris_train.csv")
iris_test <- read.csv("C:/Users/ROG/Desktop/iris_test.csv")

# Q1(a)
iris_train2 <- iris_train[, -5]
iris_test2 <- iris_test[, -5]

iris_train_n <- iris_train2
iris_test_n <- iris_test2

train_min <- apply(iris_train2, 2, min)
train_max <- apply(iris_train2, 2, max)

for (i in 1:ncol(iris_train2)) {
  iris_train_n[, i] <- (iris_train2[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  iris_test_n[, i] <- (iris_test2[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}



# Q1(b)
iris_train_labels <- iris_train$Species
iris_test_labels <- iris_test$Species

knn_predicted <- knn(train = iris_train_n, test = iris_test_n, cl = iris_train_labels, k = 3)


# Q1(c)
tb1 <- table(iris_test_labels, knn_predicted)
##                knn_predicted
## iris_test_labels setosa versicolor virginica
## setosa         15          0         0
## versicolor      0         16         2
## virginica       0          1        16

accuracy <- sum(diag(tb1)) / length(iris_test_labels)
## [1] 0.94


# Q1(d)
# The prob argument in knn() determines if the proportion of the votes for the winning class are returned as attribute prob.
# If prob = TRUE, the percentage of votes for the winning class is returned in the form of attribute prob.

knn_predicted2 <- knn(train = iris_train_n, test = iris_test_n, cl = iris_train_labels, k = 3, prob = TRUE)






# Q2
knn_L1 <- function(train, test, cl, k){
  class <- rep(0, nrow(test))
  prob <- rep(0, nrow(test))
  n <- apply(matrix(unlist(train), nrow = nrow(train), ncol = ncol(train)), 2, as.numeric)
  
  for(j in 1:nrow(test)){
    m <- apply(matrix(test[j,], nrow = nrow(train), ncol = ncol(test), byrow = TRUE), 2, as.numeric)
    d <- rowSums(abs(n - m))
    tb <- tibble("L1" = d, "label" = cl) %>% 
      arrange(L1) %>% 
      filter(L1 <= L1[k])
    result <- as.data.frame(prop.table(table(tb$label)))
    class[j] <- knitr::combine_words(as.character(result$Var1[result$Freq == max(result$Freq)]), and = ", ")
    prob[j] <- max(result$Freq)
  }
  list(class = class, prob = prob)
}




# Q3
knn_L1predicted <- knn_L1(train = iris_train_n, test = iris_test_n, cl = iris_train_labels, k = 3)
tb2 <- table(iris_test_labels, knn_L1predicted$class)
##
## iris_test_labels setosa versicolor virginica
## setosa         15          0         0
## versicolor      0         16         2
## virginica       0          1        16
## 
accuracy2 <- sum(diag(tb2)) / length(iris_test_labels)
## [1] 0.94






# Q4
wbcd_train <- read.csv("C:/Users/ROG/Desktop/wbcd_train.csv")
wbcd_test <- read.csv("C:/Users/ROG/Desktop/wbcd_test.csv")

wbcd_train2 <- wbcd_train[, -(1:2)]
wbcd_test2 <- wbcd_test[, -(1:2)]

wbcd_train_labels <- wbcd_train$diagnosis
wbcd_test_labels <- wbcd_test$diagnosis

wbcd_train_n <- wbcd_train2
wbcd_test_n <- wbcd_test2

wbcd_train_min <- apply(wbcd_train2, 2, min)
wbcd_train_max <- apply(wbcd_train2, 2, max)

for (i in 1:ncol(wbcd_train2)) {
  wbcd_train_n[, i] <- (wbcd_train2[, i] - wbcd_train_min[i]) / (wbcd_train_max[i] - wbcd_train_min[i]) 
  wbcd_test_n[, i] <- (wbcd_test2[, i] - wbcd_train_min[i]) / (wbcd_train_max[i] - wbcd_train_min[i]) 
}

knn_L1predicted_q4 <- knn_L1(train = wbcd_train_n, test = wbcd_test_n, cl = wbcd_train_labels, k = 11)
tb_knn_L1 <- table(wbcd_test_labels, knn_L1predicted_q4$class)
## 
## wbcd_test_labels  B  M
##                B 59  1
##                M  3 37
##

error_rate_knn_L1 <- 1 - sum(diag(tb_knn_L1)) / length(wbcd_test_labels)
## [1] 0.04



knn_predicted <- knn(train = wbcd_train_n, test = wbcd_test_n, cl = wbcd_train_labels, k = 11)
tb_knn <- table(wbcd_test_labels, knn_predicted)
##
##                 knn_predicted
## wbcd_test_labels  B  M
##                B 57  3
##                M  5 35
##

error_rate_knn <- 1 - sum(diag(tb_knn)) / length(wbcd_test_labels)
## [1] 0.08



wbcd_ct <- C5.0(wbcd_train2, as.factor(wbcd_train_labels))
tb_c5 <- table(wbcd_test_labels, predict(wbcd_ct, wbcd_test))
##
## wbcd_test_labels  B  M
##                B 59  1
##                M  4 36
## 

error_rate_c5 <- 1 - sum(diag(tb_c5)) / length(wbcd_test_labels)
## [1] 0.05

# KNN_L1 has the smallest error rate while KNN has the largest error rate.
# In this case, it seems that KNN_L1 has the highest accuracy.






# Q5(a)
# The code to clear objects from the workplace is rm(list = ls()).
# We need to clear the objects from the workplace because We like to clean R memory and run code again to see if there is any error or if it can reproduce the error.

# Q5(b)
# The shortcut key for sending the code to the console in Rstudio is Ctrl + Enter or Cmd + Return.

# Q5(c)
# The shortcut key for saving code in Rstudio is Ctrl + S or Cmd + S.






# Q6(a)
BTC_min <- read.csv("C:/Users/ROG/Desktop/BTCUSDT_minute.csv")
ETH_min <- read.csv("C:/Users/ROG/Desktop/ETHUSDT_minute.csv")
BTC_day <- read.csv("C:/Users/ROG/Desktop/BTCUSDT_day.csv")
ETH_day <- read.csv("C:/Users/ROG/Desktop/ETHUSDT_day.csv")

(check_BTC_min <- any(BTC_min$Volume.USDT == 0))
## [1] TRUE
(check_ETH_min <- any(ETH_min$Volume.USDT == 0))
## [1] TRUE
(check_BTC_day <- any(BTC_day$Volume.USDT == 0))
## [1] FALSE
(check_ETH_day <- any(ETH_day$Volume.USDT == 0))
## [1] FALSE
##
# It seems that there are rows with trading volume being 0 for BTC_min and ETH_min.

BTC_min_c <- filter(BTC_min, !(Volume.USDT == 0))
ETH_min_c <- filter(ETH_min, !(Volume.USDT == 0))
BTC_day_c <- BTC_day
ETH_day_c <- ETH_day


# Q6(b)
BTC_min_c$log_return <- log(BTC_min_c$close / BTC_min_c$open)
ETH_min_c$log_return <- log(ETH_min_c$close / ETH_min_c$open)
BTC_day_c$log_return <- log(BTC_day_c$close / BTC_day_c$open)
ETH_day_c$log_return <- log(ETH_day_c$close / ETH_day_c$open)


# Q6(c)
cor(BTC_day_c$log_return, ETH_day_c$log_return, method = "pearson")
## [1] 0.7711948

fit_daily <- lm(ETH_day_c$log_return ~ BTC_day_c$log_return)
summary(fit_daily)
##
## Call:
##  lm(formula = ETH_day_c$log_return ~ BTC_day_c$log_return)
##
## Residuals:
##  Min        1Q    Median        3Q       Max 
## -0.236705 -0.015539 -0.002278  0.013480  0.212241 
##
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          -0.0004503  0.0009605  -0.469    0.639    
## BTC_day_c$log_return  0.9639902  0.0220277  43.763   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.03469 on 1305 degrees of freedom
## Multiple R-squared:  0.5947,	Adjusted R-squared:  0.5944 
## F-statistic:  1915 on 1 and 1305 DF,  p-value: < 2.2e-16


# Q6(d)
cor(BTC_min_c$log_return, ETH_min_c$log_return, method = "pearson")
## [1] 0.8102123

fit_minute <- lm(ETH_min_c$log_return ~ BTC_min_c$log_return)
summary(fit_minute)
##
## Call:
##   lm(formula = ETH_min_c$log_return ~ BTC_min_c$log_return)
## 
## Residuals:
##   Min         1Q     Median         3Q        Max 
## -0.0300064 -0.0003771 -0.0000025  0.0003710  0.0206141 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          -1.459e-06  1.789e-06  -0.816    0.415    
## BTC_min_c$log_return  9.838e-01  1.388e-03 708.959   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0009177 on 263052 degrees of freedom
## Multiple R-squared:  0.6564,	Adjusted R-squared:  0.6564 
## F-statistic: 5.026e+05 on 1 and 263052 DF,  p-value: < 2.2e-16


# Q6(e)
# From fit_daily, we can see that for 1 unit increase in daily log-return of BTC, 
# the daily log-return of ETH will increase by 0.9639902 unit on average.

# From fit_minute, we can see that for 1 unit increase in 1-minute log-return of BTC, 
# the 1-minute log-return of ETH will increase by 9.838e-01 unit on average.

# The regression coefficient of daily log-return of BTC is significantly different from 0
# since its p value is very small, as is the regression coefficient of 1-minute log-return of BTC.






# Q7(a)
ggplot() +
  geom_density(data = BTC_min_c, mapping = aes(x = log_return), color = "red") +
  geom_density(data = ETH_min_c, mapping = aes(x = log_return), color = "blue")
# It looks like they are not close to a normal distribution since they have long left tails.


# Q7(b)
shapiro.test(BTC_day_c$log_return)
# p-value is small, reject the null hypothesis that the data are from a normal distribution
##
## Shapiro-Wilk normality test
##
## data:  BTC_day_c$log_return
## W = 0.89364, p-value < 2.2e-16


shapiro.test(ETH_day_c$log_return)
# p-value is small, reject the null hypothesis that the data are from a normal distribution
##
## Shapiro-Wilk normality test
##
## data:  ETH_day_c$log_return
## W = 0.91328, p-value < 2.2e-16






# Q8(a)
ggplot(data = BTC_day_c[1:100,], aes(x = Volume.USDT, y = log_return)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm")


# Q8(b)
ggplot(data = BTC_day_c[1:100,], aes(x = Volume.USDT, y = abs(log_return))) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm")


# Q8(c)
# The daily log-return of BTC and the daily trading volume in USDT are not linearly related.
# The absolute value of the daily log-return of BTC and the daily trading volume in USDT have
# a weak positive linear relationship.




# Q9(a)
n <- nrow(ETH_day_c)
day <- 30
correlation <- rep(0, n - day + 1)
for (i in 1:(n - day + 1)){
  j <- i + day -1
  correlation[i] <- cor(ETH_day_c[i:j,]$log_return, BTC_day_c[i:j,]$log_return)
}
correlation


# Q9(b)
data_label <- str_remove(ETH_day_c$date," 0:00")[rev(seq(1, n - day + 1, by = 100))]
ggplot(mapping = aes(x = 1:(n - day + 1), y = rev(correlation))) +
  geom_line() +
  scale_x_continuous(name = "Date", breaks = seq(1, n - day + 1, by = 100), labels = data_label) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  labs(y = "Correlation")






# Q10
# The daily log-return of ETH and the daily log return of BTC during most of dates have
# moderate and strong positive linear relationships since their values of correlation are 
# between 0.5 and 1, while they have weak negative linear relationships within approximately 
# 25 days prior to 10/3/2018 since their values of correlation are between -0.25 and 0.


