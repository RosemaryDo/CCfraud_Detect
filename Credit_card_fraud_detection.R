library(lattice)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(caTools)
library(readr)
library(caret)
library(ggplot2)

card = read_csv("C:/Users/thaod/Documents/STUDYING/DATA ANALYSIS/DATASET POFORLIO/FRAUD.csv")
str(card)

# convert class variable to factor
card$Class <- factor(card$Class)

set.seed(1)
split <- sample.split(card$Class, SplitRatio = 0.7)
train <- subset(card, split == T)
cv <- subset(card, split == F)

# check output Class distribution
table(cv$Class)

glm.model <- glm(Class ~ ., data = train, family = "binomial")
glm.predict <- predict(glm.model, cv, type = "response")
table(cv$Class, glm.predict > 0.5)

tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 20)
prp(tree.model) 

tree.predict <- predict(tree.model, cv, type = "class")
confusionMatrix(cv$Class, tree.predict)





#logistic regression
glm.model <- glm(Class ~ ., data = train, family = "binomial", control = list(maxit = 50))
glm.predict <- predict(glm.model, cv, type = "response")
table(cv$Class, glm.predict > 0.5)

#SVM
svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, cv)
confusionMatrix(cv$Class, svm.predict)

#Decision Tree Model
tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 20)
prp(tree.model) 

tree.predict <- predict(tree.model, cv, type = "class")
confusionMatrix(cv$Class, tree.predict)

#random forest model
set.seed(10)
rf.model <- randomForest(Class ~ ., data = train,
                         ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, cv)
confusionMatrix(cv$Class, rf.predict)

varImpPlot(rf.model)
