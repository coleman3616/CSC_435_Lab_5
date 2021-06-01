install.packages("questionr")

library(readr)
library(tidyverse)
library(dplyr)
library(caret)
library(questionr)
library(rpart)

Lost_Sales <- read_csv("Lost Sales.csv")

Lost_Sales$Status <- as.factor(Lost_Sales$Status)

set.seed(123)

## Question 1

training.samples <- Lost_Sales$Status %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- Lost_Sales[training.samples, ]
test.data <- Lost_Sales[-training.samples, ]


LR1 <- glm(Status ~ Quote, data = train.data, family = binomial)

summary(LR1)
odds.ratio(LR1)


probabilities1 <- LR1 %>% predict(test.data, type = "response")
predicted.classes1 <- ifelse(probabilities > 0.5, "Won", "Lost")
accuracy <- mean(predicted.classes1 == test.data$Status)
accuracy
error <- mean(predicted.classes != test.data$Status)
error
observed.classes <- as.factor(test.data$Status)
predicted.classes <- as.factor(predicted.classes)
confusionMatrix(predicted.classes, observed.classes, positive = "Won")

## Question 2
LR2 <- glm(Status ~ `Time to Delivery`, data = train.data, family = binomial)

summary(LR2)
odds.ratio(LR2)


probabilities2 <- LR2 %>% predict(test.data, type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.5, "Won", "Lost")
accuracy2 <- mean(predicted.classes2 == test.data$Status)
accuracy2
error2 <- mean(predicted.classes2 != test.data$Status)
error2
observed.classes2 <- test.data$Status
predicted.classes2 <- as.factor(predicted.classes2)
confusionMatrix(predicted.classes2, observed.classes2, positive = "Won")

##Question 3

training.samples2 <- Lost_Sales$Status %>% createDataPartition(p = 0.7, list = FALSE)
train.data2 <- Lost_Sales[training.samples2, ]
test.data2 <- Lost_Sales[-training.samples2, ]

LR3 <- glm(Status ~ Quote + `Time to Delivery` + `Part Type`, data = train.data2, family = binomial)

summary(LR3)
odds.ratio(LR3)

probabilities2 <- LR3 %>% predict(test.data2, type = "response")

##a.	What is the probability of winning if the Part Type is AM, Quote = $5,000, and time to delivery is 20 days?

## Question 4

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))

}

training.samples2 <- Lost_Sales$Status %>% createDataPartition(p = 0.7, list = FALSE)
train.data2 <- Lost_Sales[training.samples2, ]
test.data2 <- Lost_Sales[-training.samples2, ]

#Training
DT1 <- rpart(Status ~ Quote + `Time to Delivery` + `Part Type`, data = train.data2, method ="class")


predicted.classes3 <- DT1 %>% predict(train.data2, type = "class")
mean(predicted.classes3 == train.data2$Status)
mean(predicted.classes3 != train.data2$Status)

#Test

predicted.classes4 <- DT1 %>% predict(test.data2, type = "class")
mean(predicted.classes4 == test.data2$Status)
mean(predicted.classes4 != test.data2$Status)

##Confusion matrix
table(predicted.classes4, test.data2$Status)

##Trained DT

##Renamed variable so they will work in train
train.data2 <- train.data2 %>% rename(Time_to_Delivery=`Time to Delivery`) 
train.data2 <- train.data2 %>% rename(Part_Type=`Part Type`) 
test.data2 <- test.data2 %>% rename(Time_to_Delivery=`Time to Delivery`) 
test.data2 <- test.data2 %>% rename(Part_Type=`Part Type`) 


##tuning
DT2 <- train(Status ~ Quote + Time_to_Delivery + Part_Type, data = train.data2,
             method = "rpart", trControl = trainControl("cv", number = 10), tuneLength = 10)
##plotting tunes
plot(DT2)

##best tune
DT2$bestTune  

##best tuned model
DT2$finalModel

##best tune model plotted
plot(DT2$finalModel)

##calc accuracy and error
predicted.classes5 <- DT2 %>% predict(test.data2)
mean(predicted.classes5 == test.data2$Status)
mean(predicted.classes5 != test.data2$Status)

