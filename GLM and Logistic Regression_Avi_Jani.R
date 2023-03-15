#Avi Milan Jani
#ALY6015 - Module 3 - GLM & Logistic Regression

#Installing packages
install.packages("ISLR")
install.packages("pROC")
install.packages("caret")
install.packages("corrplot")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("correlation")

#Loading libraries
library(ISLR)
library(pROC)
library(caret)
library(corrplot)
library(dplyr)
library(ggplot2)
library(correlation)

#loading the Data set
data("College")
View(College)

# EDA
summary(College)
psych::describe(College)
str(College)
head(College)
tail(College)


#Number of Public and private universities

library(dplyr)
library(ggplot2)

College %>%
  count(Private) %>%
  ggplot(aes(x = Private, y = n, fill = Private)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Number of Public and Private Universities",
       x = "", y = "Count") +
  scale_fill_manual(values = c("#2671B2", "#4F82C4")) +
  scale_x_discrete(labels = c("Private", "Public")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none")


 
# Scatter plot of Accept vs Enroll
library(ggplot2)

ggplot(College, aes(x = Accept, y = Enroll, color = Private)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Acceptance vs Enrollment by Private/Public University",
       x = "Number of Applications", y = "Number of Enrollments") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  annotate("text", x = 40000, y = 5000, label = "Private universities", size = 4, color = "#2671B2") +
  annotate("text", x = 40000, y = 3000, label = "Public universities", size = 4, color = "#4F82C4")


# Histogram of Outstate
hist(College$Outstate, breaks=30, col="#4F82C4", xlab="Out-of-State Tuition", 
     main="Distribution of Out-of-State Tuition Fees")

# Boxplot of Room.Board vs Private
boxplot(College$Room.Board ~ College$Private, xlab="Private", ylab="Room and Board Fees",
        main="Room and Board Fees by Private/Public University", col=c("#2671B2", "#4F82C4"), 
        names=c("Public", "Private"))

#Split the data into a train and test set
library(caret)
set.seed(123)
trainIndex <- createDataPartition(College$Private, p = 0.7, list = FALSE)
train <- College[trainIndex,]
View(train)
test <- College[-trainIndex,]
View(test)


##GLM Model
glm_model <- glm(Private ~ Apps + Enroll + Grad.Rate + PhD, family = "binomial", data = train)
View(glm_model)
summary(glm_model)


#Creating Confusion Matrix
##Log-odds coefficient
coef(glm_model)

#Odds coefficient
exp(coef(glm_model))

#Making predictions
train_pred <- predict(glm_model, newdata = train, type = "response")
train_pred <- as.factor(ifelse(train_pred >=0.5, "Yes","No"))
head(train_pred)

#Accuracy of Model for the train set
confusionMatrix(train_pred, train$Private, positive = 'Yes')

#Confusion Matrix for Test data
#Predictions
test_pred <- predict(glm_model, newdata = test, type = "response")
test_pred





## GLM Model
library(stats)
College <- glm(Private ~ Apps + Enroll + Grad.Rate + PhD,
               family = "binomial", data = train)
View(College)
summary(College)

## Creating Confusion Matrix
## Log-odds coefficient
coef(College)

## Odds coefficient
exp(coef(College))

## Making predictions
ptrain <- predict(College, newdata = train, type = "response")
pl <- as.factor(ifelse(ptrain >= 0.5, "Yes", "No"))
head(pl)

## Accuracy of Model
confusionMatrix(pl, train$Private, positive = "Yes")

## Confusion Matrix for Test data
## Predictions
predicttest <- predict(College, newdata = test, type = "response")
pt <- as.factor(ifelse(predicttest >= 0.5, "Yes", "No"))
head(pt)

## Accuracy of model
confusionMatrix(pt, test$Private, positive = "Yes")

## ROC
library(pROC)
## For train data set
ROC <- roc(train$Private, ptrain)
plot(ROC, main = "ROC Curve", 
     xlab = "False Positive Rate", ylab = "True Positive Rate", col = "red")

## For test data set
ROC_test <- roc(test$Private, predicttest)
plot(ROC_test, main = "ROC Curve Test Data", 
     xlab = "False Positive Rate", ylab = "True Positive Rate", col = "blue")

## Calculating AUC
AUC_train <- auc(ROC)
AUC_test <- auc(ROC_test)
print(paste0("AUC for train data: ", AUC_train))
print(paste0("AUC for test data: ", AUC_test))

## AIC and BIC Values
print(paste0("AIC Value: ", AIC(College)))
print(paste0("BIC Value: ", BIC(College)))

## Stepwise Selection
## Stepwise logistic regression
library(MASS)

glm_fit <- stepAIC(College, direction = "both", trace = FALSE)
## Best model
glm_best <- glm(Private ~ Accept + Enroll + Outstate, data = train, family = "binomial")
summary(glm_best)

## Confusion Matrix for Test data for the best model
## Predictions
predicttest_best <- predict(glm_best, newdata = test, type = "response")
pt_best <- as.factor(ifelse(predicttest_best >= 0.5, "Yes", "No"))
head(pt_best)

## Accuracy of model
confusionMatrix(pt_best, test$Private, positive = "Yes")

## ROC for the best model
## For test data set
ROC_test_best <- roc(test$Private, predicttest_best)
plot(ROC_test_best, main = "ROC Curve Best Model Test Data", 
     xlab = "False Positive Rate", ylab = "True Positive Rate", col = "green")

## Calculating AUC
AUC_test_best <- auc(ROC_test_best)
print(paste0("AUC for the best model for test data: ", AUC_test_best))

#=======================================================================================




