setwd("C:/Users/a1310017/Desktop/PhD/Courses/Machine learning/Assignment")

library(caret)
library(randomForest)
library(rattle)


# The seed is set to assure the reproduceability of random sample
set.seed(25)

# Loading the data and setting the missing data to NA
training <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!", ""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!", ""))


# Omittin the missing values in columns of the data set
training <- training[,!apply(is.na(training), 2, any)]
testing <- testing[,!apply(is.na(testing), 2, any)]


# Cross-validation, i.e. the training data set is divided into subTraining and subTesting
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
subTraining <- training[inTrain,]
subTesting <- training[-inTrain,]



# First model uses decision tree algorithm 
model1 <- train(classe ~ .,data=subTraining,method="rpart")
pred1 <- predict(model1, subTesting)

# Prediction table
table1 <- table(pred1,subTesting$classe)
results1 <- confusionMatrix(pred1, subTesting$classe)

# Graph model1
fancyRpartPlot(model1$finalModel)

# Second model uses random forest algorithms that are shown to have greater predictive power
model2 <- train(classe ~ .,data=subTraining,method="rf",prox=TRUE)
pred2 <- predict(model2, subTesting)


# Prediction table
table2 <- table(pred2,subTesting$classe)
results2 <- confusionMatrix(pred2, subTesting$classe)


# Due to statistical propeties and illustraded performance of random forest algorithm, it is expected to perform better than simple
# decision tree algorithm. The within sample accuracy for model1 representing the decision tree is 0.7426249, while the accuracy for
# model2 representing random forest is 0.9995643. -> (model1, model2) 

# In general within sample models are overfitted, so the out-of-sample accuracy should will be lower. I expect that random forest 
# model outperformd the decision tree model both for within and out-of-sample predictions.

# The expected out-of-sample error is estimated at 0.005. The out-of-sample error is calculated as 1 - accuracy for predictions 
# made against the cross-validation set.

# The out of sample accuracy for model1 = 0.6619, while for model2 = 1. -> (results1, results2)


# Testing model2 with testing data set.
predFinal <- predict(model2, testing)



# Write files for submission
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(predFinal)