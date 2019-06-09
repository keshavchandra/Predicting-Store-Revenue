library(caret) # for dummyVars
library(RCurl) # download https data
library(Metrics) # calculate errors
library(xgboost) # model
library(MASS) 
library(Metrics)
library(corrplot)
library(randomForest)
library(lars)
library(ggplot2)
library(xgboost)
library(Matrix)
library(methods)
#load the csv train and test files into train1 and test1 respectively. 
train1=read.csv(choose.files(),header = TRUE,stringsAsFactors = T)
test1=read.csv(choose.files(),header = TRUE,stringsAsFactors = T)
#count the null values and delete the rows
for(i in c(1:56))
{
  count=0;
  count<-sum(is.na(train1[,i]))/1693;
  if(count>.8)
  {
    print(i);
  }
}
#As the count is very less,we will not delete the attributes.

#test data cleaning
#have set all other null values to zero
test1[is.na(test1)]<-0
#change categorical data into numeric data
for(i in 1:48){
  if(is.factor(test1[,i])){
    test1[,i]<-as.integer(test1[,i])
  }
}
#train data
#train data cleaning
train1[is.na(train1)]<-0
#convert categorical data into the numeric data
for(i in 1:49){
  if(is.factor(train1[,i])){
    train1[,i]<-as.integer(train1[,i])
  }
}
#divided the training data into 2 sets Training_Inner and Testing_Inner to check the model 
Training_Inner<- train1[1:floor(length(train1[,1])*0.8),]
## We have done crossm validation for the Test_Inner values which is 20 percent of the train data.
Test_Inner<- train1[(length(Training_Inner[,1])+1):1680,]

set.seed(10)
param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse",
              "eta" = 0.1, "max.depth" = 11)
xgb.cv = xgb.cv(param=param,data = as.matrix(train1[,3:49]),seed=1, label=train1$REVENUE_2013,nfold = 40, nrounds = 400)
plot(log(xgb.cv$test.rmse.mean),type = "l")
xgb <- xgboost(data = as.matrix(train1[,3:49]), label =train1$REVENUE_2013, max.depth = 11, 
               eta = 0.1, nround = 600,
               nthread = 2,seed=1, objective = "reg:linear")

trees = xgb.model.dt.tree(dimnames(train1[,3:49])[[2]],model = xgb)
trees
#to display the most important attributes of the dataset
model <- xgb.dump(bst, with.stats = T)
model[1:10]
names <- dimnames(train1[,3:49])[[2]]
names
importance_matrix <- xgb.importance(names, model = xgb)
importance_matrix
#plot the importance matrix which will display the 10 most important attributes.
xgb.plot.importance(importance_matrix[1:10,])
xgb.plot.tree(feature_names = names, model = xgb, n_first_tree = 2)
#Then following code is used for the final prediction of the REVENUE_2013  from the train model xgb for the test data.
y_pred <- predict(xgb, data.matrix(test1[,2:48]))
y_pred
#convert the data into csv file and export it
write.csv(y_pred,file="C:/Users/Hp pc/Desktop/competetion/mid/mycds.csv")
