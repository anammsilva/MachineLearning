<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<style> 
div.test2 {
	width: 200px;
	padding: 10px;
	border: 5px solid #000000;
<!--	margin-left: 10%; -->
	margin-bottom: 10px;
	margin-top: 10px;
}

div.test {
	width: 600px;
	padding: 2px;
	border: 2px solid #000000;
<!--	margin-left: 10%; -->
	margin-bottom: 10px;
	margin-top: 10px;
}

div {
   word-wrap: break-word;
   width: 700px;
}
</style>
</head>
<body>

<div><p>The aim of this document is to describe the program implemented to predict the behavior - how well they are going - of six participants when they are doing physical exercise, based on the data collected from accelerometers.</p></div>

<div><p><font size="2">For that, you should download the train and test sets about human activity recognition and put them in the same folder. Then, you choose that folder as your working directory and import the data.</font></p></div>

<div class="test">
<p><center>train <- read.csv("./pml-training.csv", na.strings= c("NA"," ","na", ""))</center></p>
<p><center>test <- read.csv("./pml-testing.csv", na.strings= c("NA"," ","na", ""))</center></p>
<p><center>dim(train)</center></p>
<p><center>dim(test)</center></p>
</div>

<div><p><font size="2">We verify that we have a train matrix with dimension 19622*160 and a test matrix with 20*160. The only difference in the columns is that in the training data, the last column is the class of each observation and in the test data is the problem id. 
We required the packages needed to run the program.</font></p></div>

<div class="test">
<p><center>library(caret)</center></p>
<p><center>library(randomForest)</center></p>
<p><center>library(kernlab)</center></p>
<p><center>library(rpart)</center></p>
</div>


<div><p>After that, we will pre-process the training data, counting the number of NA’s and then remove the columns with more than 20% of NA observations. If some column has less than 20%, we substitute the values for the mean value of that column</p></div>

<div class="test">
<p>train.na <- apply(train, 2, function(a) {sum(is.na(a))})</p>
<p>train_data<- train[,which(train.na <=0.2*dim(train)[2])]</p>
<p>train_data[is.na(train_data)]<-mean(train_data[,-c(is.na(train_data))])</p>
</div>

<div><p>Now, we had a train matrix with dimension 19622*60. After that, we split the training set into train data (70%) and cross validation data (30%). We will use this 30% to evaluate our models.</p></div>

<div class="test">
<p>partition <- createDataPartition(y = train_data$classe, p = 0.7, list = FALSE)</p>
<p>train <- train_data[partition, ]</p>
<p>cv<- train_data[-partition, ]</p>
</div>

<div><p>After that, we use <i>random forest</i> to evaluate the importance of the predictors in the training data, using mean decreasing accuracy.We will not use the last 10 variables in this order, because they are less important than the others.</p></div>

<div class="test">
<p>m <- randomForest(classe ~ ., data = train, keep.forest=FALSE, 
importance=T, ntree=1000)</p>
<p>ord<-order(importance(m, type=1))</p>
<p>varImpPlot(m, n.var=10)</p>
<p>train<-train[,-c(ord[(length(ord)-9):length(ord)])]</p>
</div>

<div><p>Finally, we’ll use different learning algorithms to train this data: <i>Random forests, Support Vector Machines and Decision Trees</i>, using a seed.</p></div>

<div class="test">
<p><center>set.seed(1234)</center></p>
<p><center>RF <- randomForest(classe ~ ., data = train)</center></p>
<p><center>SVM<-ksvm(classe ~., data=train)</center></p>
<p><center>DT<-train(classe ~ .,method="rpart",data=train)</center></p>
</div>

<div><p>We evaluate each algorithm in the remaining 30% of the data, using confusion Matrix to show the results.</p></div>

<div class="test">
<p><center>predictRF <- predict(RF, cv)</center></p>
<p><center>confusionMatrix(cv$classe, predictRF)</center></p>
<p><center>predictSVM <- predict(SVM, cv)</center></p>
<p><center>confusionMatrix(cv$classe, predictSVM)</center></p>
<p><center>predictDT <- predict(DT, cv)</center></p>
<p><center>confusionMatrix(cv$classe, predictDT)</center></p>
</div>

<div><p>We got 98.9% accuracy for RF, 90.5% for SVM, and 52.9% for DF. So, RF is the most robust, and we will use it to predict test data. We will train <i>Random Forest</i> again, but now, training with all the data and using the same seed.</p></div>

<div class="test">
<p><center>train_all<-train[,-c(ord[(length(ord)-9):length(ord)])]</center></p>
<p><center>set.seed(1234)</center></p>
<p><center>RF <- randomForest(classe ~ ., data = train_all)</center></p>
</div>

<div><p>Now, we end predicting the test data using this algorithm.</p></div>

<div class="test">
<p><center>answers <- predict(RF, test))</center></p>
</div>

<div><p>We think the model is quite robust because we removed the less important variables which could introduce noise in the data. We test three different algorithms and we choose the best one, with a higher accuracy. As the cross validation data is independent from the training data, we expect the out of sample error to be similar to that one obtained in CV.</p></div>

</div>

</body>
</html>
