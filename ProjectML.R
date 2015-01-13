library(caret)
library(randomForest)
library(kernlab)
library(rpart)


#1. choose the directory where you have the training and test sets

#import the training data from the csv file
train <- read.csv("./pml-training.csv", na.strings= c("NA"," ","na", ""))

#import the test data from the csv file
test <- read.csv("./pml-testing.csv", na.strings= c("NA"," ","na", ""))

#evaluating the quality of the data and cleaning it

#count the number of NA for column in the training data
train.na <- apply(train, 2, function(a) {sum(is.na(a))})

#remove the columns with more than 20% NA's observations
train_data<- train[,which(train.na <=0.2*dim(train)[2])]

#If there are less than 20% NA's in the column, we substitute that values for the column mean value
train_data[is.na(train_data)]<-mean(train_data[,is.na(train_data)])

#divide training data into training (70%) and CV (30%) sets
partition <- createDataPartition(y = train_data$classe, p = 0.7, list = FALSE)
train <- train_data[partition, ]
cv<- train_data[-partition, ]


#Using Random Forest to evaluate variables' importance
m <- randomForest(classe ~ ., data = train, keep.forest=FALSE, importance=T, ntree=1000)
ord<-order(importance(m, type=1))
varImpPlot(m, n.var=10)


#we will not use the last 10 variables of the ranking by Mean Decrease Accuracy
train<-train[,-c(ord[(length(ord)-9):length(ord)])]


#construct a model that uses all the other variables to predict the 
#class label using diferent learning algorithms
set.seed(1234)
#Random Forests
RF <- randomForest(classe ~ ., data = train)
#Support Vector Machines
SVM<-ksvm(classe ~., data=train)
#Decision Tree
DT<-train(classe ~ .,method="rpart",data=train)

############################

#Apply cross validation to the other 30% data to evaluate the best learning algorithm 

predictRF <- predict(RF, cv)
confusionMatrix(cv$classe, predictRF)
predictSVM <- predict(SVM, cv)
confusionMatrix(cv$classe, predictSVM)
predictDT <- predict(DT, cv)
confusionMatrix(cv$classe, predictDT)

#Best one: Random Forest with Accuracy: 0.99 

#using all the training data to train the algorithm

train_all<-train[,-c(ord[(length(ord)-9):length(ord)])]

RF <- randomForest(classe ~ ., data = train_all)

# predict the classes of the test set with the best model
answers <- predict(RF, test)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
