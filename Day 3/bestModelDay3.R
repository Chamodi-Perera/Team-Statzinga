#did feature engineering part in excel and uploaded the data set.
# pay_month(due amount_month-paid ampount_month) for all 6 months. these extra variables were added to the model

#load data seet and remove ID Column
setwd("C:\\Users\\sekha\\Desktop\\data-storm-10")
getwd()
data=read.csv("FEATURE ENG.csv")
realtest=read.csv("FEATURE ENG test.csv")
summary(data)
data=data[,-1]

#create the necessary categorical variables

levels=c("-2","-1","0","1","2","3", "4","5","6", "7", "8","9")
data$NEXT_MONTH_DEFAULT=factor(data$NEXT_MONTH_DEFAULT)
data$PAY_JULY=factor(data$PAY_JULY,levels=levels)
data$PAY_AUG=factor(data$PAY_AUG,levels=levels)
data$PAY_SEP=factor(data$PAY_SEP,levels=levels)
data$PAY_OCT=factor(data$PAY_OCT,levels=levels)
data$PAY_NOV=factor(data$PAY_NOV,levels=levels)
data$PAY_DEC=factor(data$PAY_DEC,levels=levels)

#remove id column from test set and make the corrrect data format
realtest=realtest[,-1]

levels=c("-2","-1","0","1","2","3", "4","5","6", "7", "8","9")
realtest$PAY_JULY=factor(realtest$PAY_JULY,levels=levels)
realtest$PAY_AUG=factor(realtest$PAY_AUG,levels=levels)
realtest$PAY_SEP=factor(realtest$PAY_SEP,levels=levels)
realtest$PAY_OCT=factor(realtest$PAY_OCT,levels=levels)
realtest$PAY_NOV=factor(realtest$PAY_NOV,levels=levels)
realtest$PAY_DEC=factor(realtest$PAY_DEC,levels=levels)


#separate test set and training set
set.seed(1234)
ind=sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
train=data[ind==1,]
test=data[ind==2,]

#cross validate model for random forest using a custom RF in caret package

library(caret)
set.seed(1234)
folds=createMultiFolds(data$NEXT_MONTH_DEFAULT,k=5,times=10)
ctrl=trainControl(method="repeatedcv",number=5,repeats=10,index=folds)

library(randomForest)

library(randomForest)  # fitting random forest
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# after creating a custom rf algorithm cross validate search for best model
tunegrid=expand.grid(.mtry=c(1:11),.ntree=c(1:10))


model5=train(data[-12],data$NEXT_MONTH_DEFAULT,method = customRF, trControl=ctrl, tuneGrid=tunegrid)
model5$bestTune
model=model5$finalModel
set.seed(1)

#find best model classificaton matrix
predict=predict(model,test)
tab2=table(predicted=predict, actual=test$NEXT_MONTH_DEFAULT)
tab2
sum(diag(tab2))/sum(tab2)

#spply model to the full training set
set.seed(1)
model=randomForest(NEXT_MONTH_DEFAULT~., mtry=6,ntree=158, 
                   maxnodes=33,data=data, importance=T)

#predict to the test set and write csv file
prediction=predict(model,realtest)
write.table(prediction, file="sub1.csv", sep=",")
