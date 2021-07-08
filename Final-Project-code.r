library(tidyverse)
library(caret)
library(gmodels)
library(pROC)
library(mlbench)
library(nnet)
library(ggpubr)
library(dplyr)
library(tictoc)
library(C50)
library(class)
library(neuralnet)
library(fastDummies)
library(MASS)
library(randomForest)




data <- read_csv('ObesityDataSet_raw_and_data_sinthetic.csv')

names(data)
## [1] "Gender"                         "Age"                           
## [3] "Height"                         "Weight"                        
## [5] "family_history_with_overweight" "FAVC"                          
## [7] "FCVC"                           "NCP"                           
## [9] "CAEC"                           "SMOKE"                         
## [11] "CH2O"                           "SCC"                           
## [13] "FAF"                            "TUE"                           
## [15] "CALC"                           "MTRANS"                        
## [17] "NObeyesdad"  




# Check missing point
apply(data,2,function(x) sum(is.na(x)))
## Gender                            Age                         Height 
##      0                              0                              0 
## Weight family_history_with_overweight                           FAVC 
##      0                              0                              0 
## FCVC                            NCP                           CAEC 
##    0                              0                              0 
## SMOKE                           CH2O                            SCC 
##     0                              0                              0 
## FAF                            TUE                           CALC 
##   0                              0                              0 
## MTRANS                     NObeyesdad 
##      0                              0 




#################### Multinomial Log-linear regression Regression ####################
# 1. Data Processing 
data <- read_csv('ObesityDataSet_raw_and_data_sinthetic.csv')

# Remove height and weight
df <- data[, -c(3:4)]

df$NObeyesdad <- factor(df$NObeyesdad, levels = c("Insufficient_Weight", "Normal_Weight",  "Overweight_Level_I", "Overweight_Level_II",  "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
df$CAEC <- factor(df$CAEC, levels = c("no", "Sometimes", "Frequently", "Always"))
df$CALC <- factor(df$CALC, levels = c("no", "Sometimes", "Frequently", "Always"))
df$family_history_with_overweight <- as.factor(df$family_history_with_overweight)
df$Gender <- as.factor(df$Gender)
df$FAVC <- as.factor(df$FAVC)
df$FCVC <- as.factor(round(df$FCVC))
df$SMOKE <- as.factor(df$SMOKE)
df$CH2O <- as.factor(round(df$CH2O))
df$SCC <- as.factor(df$SCC)
df$FAF <- as.factor(round(df$FAF))
df$TUE <- as.factor(round(df$TUE))
df$MTRANS <- as.factor(df$MTRANS)

summary(df)

# 2. Partition data
set.seed(362)
random_index <- sample(nrow(df), round(nrow(df) * 0.7)) 
data_train <- df[random_index, ]
data_test <- df[-random_index, ]



# 3. Multinomial Log-linear regression Regression
fit_1 <- multinom(NObeyesdad ~., data = data_train)

step(fit_1, trace = 0)

# let the final output from step function as fit_2
fit_2 <- multinom(NObeyesdad ~ Gender + Age + family_history_with_overweight + 
                    FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + 
                    CALC + MTRANS,data = data_train)

anova(fit_2, fit_1)

summary(fit_2)

exp(coef(fit_2))

# 4. 2 tailed z Test
z <- summary(fit_2)$coefficients/summary(fit_2)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
p


# 5. Prediction and performance test
predicted_NObeyesdad <- predict(fit_2, data_test)

table(data_test$NObeyesdad, predicted_NObeyesdad)


mean(predicted_NObeyesdad == data_test$NObeyesdad)
## [1] 0.6192733

CrossTable(predicted_NObeyesdad, data_test$NObeyesdad, prop.chisq = FALSE) 


Overall_Statistics <- confusionMatrix(predicted_NObeyesdad, data_test$NObeyesdad)[[3]]
as.data.frame(Overall_Statistics)

Statistics_by_Class <- as.data.frame(confusionMatrix(predicted_NObeyesdad, data_test$NObeyesdad)[[4]])[,1:2]


# Cross-validation
k.folds <- function(k) {
  folds <- createFolds(df$NObeyesdad, k = k, list = TRUE, returnTrain = TRUE)
  accuracy <- rep(0, k)
  for (i in 1:k) {
    train_data <- df[folds[[i]],]
    test_data <- df[-folds[[i]],]
    fit_sample <- multinom(NObeyesdad ~ Gender + Age + family_history_with_overweight + 
                             FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + 
                             CALC + MTRANS, data = train_data, method = "class")
    prediction <- predict(fit_sample, test_data, type = "class")
    accuracy[i] <- mean(prediction == test_data$NObeyesdad)
  }
  accuracy
}

set.seed(500)
accuracies <- k.folds(10) 
## [1] 0.6037736 0.5943396 0.6238095 0.6208531 0.6303318 0.6142857 0.5943396 0.6398104 0.6333333 0.6037736

mean(accuracies)
## [1] 0.615865



####################################################################################################
# Data Visualization
data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")

# Male vs Female

male_count <- data %>% 
  filter(Gender == "Male") %>% 
  ggplot(aes(x = NObeyesdad)) +
  geom_bar(stat = "count", fill = "lightblue") +
  labs(title = "Male", x = "Condition")

female_count <- data %>% 
  filter(Gender == "Female") %>% 
  ggplot(aes(x = NObeyesdad)) +
  geom_bar(stat = "count", fill = "pink") +
  labs(title = "Female", x = "Condition")

male_vs_female_count <- ggarrange(male_count, female_count)

# Smoker vs Non Smoker

smoker_count <- data %>% 
  filter(SMOKE == "yes") %>% 
  ggplot(aes(x = NObeyesdad)) +
  geom_bar(stat = "count", fill = "grey") +
  labs(title = "Smokers", x = "Condition")

non_smoker_count <- data %>% 
  filter(SMOKE == "no") %>% 
  ggplot(aes(x = NObeyesdad)) +
  geom_bar(stat = "count", fill = "beige") +
  labs(title = "Non Smokers", x = "Condition")

smoker_vs_non_smoker_count <- ggarrange(smoker_count, non_smoker_count)

# Family History vs No Family History

hist_count <- data %>% 
  filter(family_history_with_overweight == "yes") %>% 
  ggplot(aes(x=NObeyesdad)) +
  geom_bar(stat = "count", fill = "lightgreen") +
  labs(title = "Family History with Overweight", x = "Condition")

no_hist_count <- data %>% 
  filter(family_history_with_overweight == "no") %>% 
  ggplot(aes(x=NObeyesdad)) +
  geom_bar(stat = "count", fill = "darkgreen") +
  labs(title = "No Family History with Overweight", x = "Condition")

hist_vs_no_hist_count <- ggarrange(hist_count, no_hist_count)

# Food Between Meals Comparison

always_between_meals <- data %>% 
  filter(CAEC == "Always") %>% 
  ggplot(aes(x=NObeyesdad)) +
  geom_bar(stat = "count", fill = "darkred") +
  labs(title = "Always Eats between Meals", x = "Condition")

frequently_between_meals <- data %>% 
  filter(CAEC == "Frequently") %>% 
  ggplot(aes(x=NObeyesdad)) +
  geom_bar(stat = "count", fill = "red") +
  labs(title = "Frequently Eats between Meals", x = "Condition")

sometimes_between_meals <- data %>% 
  filter(CAEC == "Sometimes") %>% 
  ggplot(aes(x=NObeyesdad)) +
  geom_bar(stat = "count", fill = "pink") +
  labs(title = "Sometimes Eats between Meals", x = "Condition")

no_between_meals <- data %>% 
  filter(CAEC == "no") %>% 
  ggplot(aes(x=NObeyesdad)) +
  geom_bar(stat = "count", fill = "lightpink") +
  labs(title = "Never Eats between Meals", x = "Condition")

eat_between_meals_comparison <- ggarrange(always_between_meals, frequently_between_meals,
                                          sometimes_between_meals, no_between_meals)

ggplot(data, aes(x= NObeyesdad, y= Age))+
  geom_boxplot()
####################################################################################################

# Classification

# Data wrangling and normalization for classification

data <- read_csv('ObesityDataSet_raw_and_data_sinthetic.csv')

df <- data

df.new = dummy_cols(df[, -17], remove_first_dummy = TRUE, remove_selected_columns = TRUE)

df = df.new[, -c(2, 3)]
df$NObeyesdad = data$NObeyesdad

df$NObeyesdad <- factor(df$NObeyesdad, levels = c("Insufficient_Weight", "Normal_Weight", 
                                                  "Overweight_Level_I", "Overweight_Level_II", 
                                                  "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

set.seed(362)
random_index <- sample(nrow(df), round(nrow(df) * 0.7)) 
data_train <- df[random_index, ]
data_test <- df[-random_index, ]


# normalize data 

normalize <- function(train, test) {
  train_n <- train
  test_n <- test
  
  train_min <- apply(train, 2, min)
  train_max <- apply(train, 2, max)
  
  for (i in 1:ncol(train)) {
    train_n[, i] <- (train[, i] - train_min[i]) / (train_max[i] - train_min[i])
    
    # use the min and max from training data to normalize the testing data
    test_n[, i] <- (test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  }  
  
  return(list(train = train_n, test = test_n))
}

train_test_n = normalize(data_train[, -22], data_test[, -22])

train_n = train_test_n$train
test_n = train_test_n$test

# KNN

train_labels = data_train$NObeyesdad
test_labels = data_test$NObeyesdad

knn_predicted = knn(train = train_n, test = test_n,
                    cl = train_labels, k = 3)

confusionMatrix(data = knn_predicted, reference = data_test$NObeyesdad)

#C50

classtree = C5.0(x = data_train[, -22], y = data_train$NObeyesdad)
confusionMatrix(data = predict(classtree, data_test), reference = data_test$NObeyesdad)

# NeuralNet

train_n$NObeyesdad = data_train$NObeyesdad
test_n$NObeyesdad = data_test$NObeyesdad

train.n = dummy_cols(train_n, select_columns = "NObeyesdad", remove_selected_columns = TRUE)
test.n = dummy_cols(test_n, select_columns = "NObeyesdad", remove_selected_columns = TRUE)

tic()
fit = neuralnet(NObeyesdad_Insufficient_Weight + NObeyesdad_Normal_Weight + NObeyesdad_Overweight_Level_I + NObeyesdad_Overweight_Level_II + NObeyesdad_Obesity_Type_I + NObeyesdad_Obesity_Type_II + NObeyesdad_Obesity_Type_III ~ ., 
                data = train.n, hidden = c(4, 4), linear.output = FALSE, threshold = 0.1, stepmax = 500000)
toc()

predict <- max.col(neuralnet::compute(fit, test_n)$net.result)

predict <- recode(predict, "1" = "Insufficient_Weight", "2" = "Normal_Weight", "3" = "Overweight_Level_I", 
                  "4" = "Overweight_Level_II", "5" = "Obesity_Type_I", "6" = "Obesity_Type_II",
                  "7" = "Obesity_Type_III")

predict <- factor(predict, levels = c("Insufficient_Weight", "Normal_Weight", 
                                      "Overweight_Level_I", "Overweight_Level_II", 
                                      "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

confusionMatrix(data = predict, reference = test_n$NObeyesdad)


#################### hypotheses and perform hypothesis testing ####################
data <- read_csv('ObesityDataSet_raw_and_data_sinthetic.csv')

##data visulization figure 6
ggplot(DataObesity, aes(x = NObeyesdad, y= Height ))+
  geom_jitter()
########### Formulate some hypotheses and perform hypothesis testing
##get some subsets by using dplyr library
TypeI <- data %>% filter(NObeyesdad == "Obesity_Type_I")
TypeII <- data %>% filter(NObeyesdad == "Obesity_Type_II")
TypeIII <- data %>% filter(NObeyesdad == "Obesity_Type_III")
DataObesity <- data %>% filter(NObeyesdad == "Obesity_Type_I" | NObeyesdad == "Obesity_Type_II" | NObeyesdad == "Obesity_Type_III")
overweight1 <- data %>% filter(NObeyesdad == "Overweight_Level_I")
overweight2 <- data %>% filter(NObeyesdad == "Overweight_Level_II")
insufficientweight <- data %>% filter(NObeyesdad == "Insufficient_Weight")

## Comparing the mean of two samples
#if average age for people who have Obesity_type_I and Obesity_type_II same
t.test(data$Age[data$NObeyesdad == "Obesity_Type_I"], 
       data$Age[data$NObeyesdad == "Obesity_Type_II"])
t.test(data$Age[data$NObeyesdad == "Obesity_Type_II"], 
       data$Age[data$NObeyesdad == "Obesity_Type_III"])
#t = -4.6868, df = 598.41, p-value = 3.439e-06
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#        -3.33310 -1.36459
#sample estimates:
#        mean of x mean of y mean of z
#25.88494  28.23379 23.49555 

#if average height for people who have Obesity_Type_I and Obesity_typeIII same
t.test(data$Height[data$NObeyesdad == "Obesity_Type_I"], 
       data$Height[data$NObeyesdad == "Obesity_Type_II"])
t.test(data$Height[data$NObeyesdad == "Obesity_Type_II"], 
       data$Height[data$NObeyesdad == "Obesity_Type_III"])
t.test(data$Height[data$NObeyesdad == "Obesity_Type_III"], 
       data$Height[data$NObeyesdad == "Normal_Weight"])
#t = 0.97833, df = 612.33, p-value = 0.3283
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
##        -0.006290334  0.018779148
#sample estimates:
#        mean of x mean of y 
#1.693804  1.771795  1.687559

#if average weight for people who have Obesity_Type_II and Obesity_Type_III the same
t.test(data$Weight[data$NObeyesdad == "Obesity_Type_I"], 
       data$Weight[data$NObeyesdad == "Obesity_Type_II"])
t.test(data$Weight[data$NObeyesdad == "Obesity_Type_II"], 
       data$Weight[data$NObeyesdad == "Obesity_Type_III"])
t.test(data$Weight[data$NObeyesdad == "Obesity_Type_III"], 
       data$Weight[data$NObeyesdad == "Overweight_Level_I"])
t.test(data$Weight[data$NObeyesdad == "Overweight_Level_I"], 
       data$Weight[data$NObeyesdad == "Overweight_Level_II"])
#t = -5.7478, df = 492.88, p-value = 1.586e-08
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#        -7.562297 -3.709309
#sample estimates:
#        mean of x mean of y 
# 92.8702 115.3053  120.9411 


##
# Perform t-test
#For Type I Obesity patients, whether the average consumption of water daily is equal to two
#while the rest of there are three levels from 1 to 3
t.test(TypeI$CH2O, mu = 2)  #mean of x = 2.112218 
t.test(TypeII$CH2O, mu = 2)  # 1.877658 
t.test(TypeIII$CH2O, mu = 2)  #2.208493 
# Interval Estimation
#the 99% confidence interval of Obesity patients' (with three types together) average number of main meals
t.test(DataObesity$NCP, conf.level = 0.99)  #99 percent confidence interval: 2.665547, 2.768026


##Are the means of the weights in the smoking group and non-smoking group different?
t.test(data$Weight[data$SMOKE == "yes"], data$Weight[data$SMOKE == "no"])
#yes, mean of smoker, mean of non_smoker: 91.20681  86.48770 
# Is it same for obesity patients?
t.test(DataObesity$Weight[DataObesity$SMOKE == "yes"], DataObesity$Weight[DataObesity$SMOKE == "no"])
#yes, 114.5045 > 108.9568 

##Are the means of the weights for overweight level I people to overweight level III people who have family history with overweight differenti?
t.test(overweight1$Weight[data$family_history_with_overweight == "yes"], overweight1$Weight[data$family_history_with_overweight == "no"])
t.test(overweight2$Weight[data$family_history_with_overweight == "yes"], overweight2$Weight[data$family_history_with_overweight == "no"])
#yes, the means are:   74.82933  73.09394 :  82.60761  80.99613 


#prop.test, if three types of obesity people have equal portion of people who use Automobile as transportation
no_automobile <- c(sum(TypeI$MTRANS == "Automobile"), sum(TypeII$MTRANS == "Automobile"),
                   sum(TypeIII$MTRANS == "Automobile")) # no. of smokers in the 3 groups
no_trails_threeTypesofObesity <- c(nrow(TypeI), nrow(TypeII),
                                   nrow(TypeIII)) # corresponding no. of total people in the 3 groups
prop.test(no_automobile, no_trails_threeTypesofObesity)
#    prop 1     prop 2     prop 3: 0.31339031 0.31986532 0.00308642 
#we could see that Type I and Type II groups have similar proportions, however, Type III group doesn't have that many people who use automobile for transportation

#We want to know if the difference between the two proportions is statistically significant.
##for smokers in type I Obesity patients and type II Obesity patients
prop.test(c(sum(TypeI$SMOKE == "yes"), sum(TypeII$SMOKE == "yes")), c(nrow(TypeI), nrow(TypeII)))
0.01709402-0.05050505  #-0.03341103
#prop1, prop2: 0.01709402 0.05050505, we could see that the difference is only 0.033, not much difference

##As numeric
ks.test(data$Weight[data$NObeyesdad == "Insufficient_Weight"], data$Weight[data$NObeyesdad == "Overweight_Level_II"])
2.2e-16 < 0.05 #TRUE
#The  p-value < 2.2e-16, which is smaller than  0.05. Therefore, we conclude that the difference of the two distributions is statistically significant

################################## Random Forest Classification #####################
###Statistics Analysis type 1
# 1. Data Processing 
df <- data
df <- df[,-c(3, 4)]
df$NObeyesdad <- factor(df$NObeyesdad, levels = c("Insufficient_Weight", "Normal_Weight",  "Overweight_Level_I", "Overweight_Level_II",  "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
df$CAEC <- factor(df$CAEC, levels = c("Always", "Frequently", "Sometimes", "no"))
df$CALC <- factor(df$CALC, levels = c("Always", "Frequently", "Sometimes", "no"))
df$family_history_with_overweight <- as.factor(df$family_history_with_overweight)
df$Gender <- as.factor(df$Gender)
df$FAVC <- as.factor(df$FAVC)
df$FCVC <- as.factor(round(df$FCVC))
df$NCP <- as.factor(round(df$NCP))
df$SMOKE <- as.factor(df$SMOKE)
df$CH2O <- as.factor(round(df$CH2O))
df$SCC <- as.factor(df$SCC)
df$FAF <- as.factor(round(df$FAF))
df$TUE <- as.factor(round(df$TUE))
df$MTRANS <- as.factor(df$MTRANS)
# 2. Partition data
set.seed(362)
random_index <- sample(nrow(df), round(nrow(df) * 0.7)) 
data_train <- df[random_index, ]
data_test <- df[-random_index, ]

##################### Random Forest Classification 
tic()
fitrf = randomForest(NObeyesdad ~ ., data = data_train, 
                     importance = TRUE, 
                     mtry = 3, 
                     ntree=100 ) 
toc()
# Prediction and performance test
yhat.test = predict(fitrf, data_test)
tablerf <- table(yhat.test, data_test$NObeyesdad)
accuracyrf <- mean(data_test$NObeyesdad == yhat.test) ##0.8056872
### misclassification error is about 80.57%.
write.table(tablerf, file = "tablerf.txt", sep = ",", quote = FALSE, row.names = TRUE)
# Cross-validation
k_folds <- function(k) {
  folds <- createFolds(df$NObeyesdad, k = k, list = TRUE, returnTrain = TRUE)
  accuracy <- rep(0, k)
  for (i in 1:k) {
    train_data <- df[folds[[i]],]
    test_data <- df[-folds[[i]],]
    fit_sample <- randomForest(NObeyesdad ~ ., data = data_train, 
                               importance = TRUE, 
                               mtry = 3, 
                               ntree=100) 
    prediction <- predict(fit_sample, test_data, type = "class")
    accuracy[i] <- mean(prediction == test_data$NObeyesdad)
  }
  accuracy
}
set.seed(500)
accuracy <- k_folds(10) 
mean(accuracy) 
#[1] 0.9075928

confusionMatrix(yhat.test, data_test$NObeyesdad)

###Statistics Analysis type 2
###Ordinal Logistic Regression  #####TESTING ONLY
# In order to run the ordinal logistic regression model, we need the polr function from the MASS package
# Making frequency table between the response variable and predict variable Consumption of food between meals (CAEC)
table(data_train$NObeyesdad, data_train$CAEC)
tic()
# Build ordinal logistic regression model
fitordinal <- polr(NObeyesdad ~ ., data = data_train, Hess = TRUE)
toc()
#get the predicted result 
predicted_ordinal <- predict(fitordinal, data_test)
mean(data_test$NObeyesdad == predicted_ordinal) ##0.4265403
###misclassification error is about 42.65%.
# Cross-validation
k_folds <- function(k) {
  folds <- createFolds(df$NObeyesdad, k = k, list = TRUE, returnTrain = TRUE)
  accuracy <- rep(0, k)
  for (i in 1:k) {
    train_data <- df[folds[[i]],]
    test_data <- df[-folds[[i]],]
    fit_sample <- polr(NObeyesdad ~ ., data = data_train, Hess = TRUE)
    prediction <- predict(fit_sample, test_data, type = "class")
    accuracy[i] <- mean(prediction == test_data$NObeyesdad)
  }
  accuracy
}
set.seed(500)
accuracy <- k_folds(10) 
mean(accuracy)
#0.3869922
