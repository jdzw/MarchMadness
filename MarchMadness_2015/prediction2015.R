setwd("~/Personal/MarchMadness_2015")
# source("2015_utility.R")
source("2015_utility_v2.R")
library(tree)
library(randomForest)
library(gamlr)


#### Import files ####
# regSeason <- read.csv("inputs/RegularSeasonCompactResults.csv",header=TRUE,stringsAsFactors = FALSE)
regSeason <- read.csv("inputs/RegularSeasonDetailedResults.csv",header=TRUE,stringsAsFactors = FALSE)
seasons <- read.csv("inputs/Seasons.csv",header=TRUE,stringsAsFactors = FALSE)
teams <- read.csv("inputs/Teams.csv",header=TRUE,stringsAsFactors = FALSE)
tourneyRes <- read.csv("inputs/TourneyCompactResults.csv",header=TRUE,stringsAsFactors = FALSE)
tourneySeeds <- read.csv("inputs/TourneySeeds.csv",header=TRUE,stringsAsFactors = FALSE)
tourneySlots <- read.csv("inputs/TourneySlots.csv",header=TRUE,stringsAsFactors = FALSE)

ratings <- read.csv("inputs/massey_ordinals_2003-2015.csv")
ratings <- reshape(ratings,timevar="sys_name",idvar=c("season","rating_day_num","team"),direction="wide")

# Other metrics to include...
"Win Percentage in last 4 weeks
Win Percentage against playoff teams"

#### Create full training data set ####
train_data_frame <- data.frame()
for(i in 2003:2011) {
  train_data_frame <- rbind(train_data_frame,train_frame_model(i))
  print(i)
}
train_data_frame_clean <- train_data_frame[,colSums(is.na(train_data_frame)) == 0]

#### Apply Classification Tree Model ####
library("rpart")
train_rpart <- rpart(Win ~ A_TWPCT + A_WST6 + A_SEED + A_AWAYPCT + A_CloseWin + A_CloseLoss + A_BigWin + 
                       A_BigLoss + B_TWPCT + B_WST6 + B_SEED + B_AWAYPCT + B_CloseWin + B_CloseLoss + 
                       B_BigWin + B_BigLoss, data=train_data_frame, method="class")


win <- train_data_frame_clean[,2]
train_tree <- tree(as.factor(win) ~ ., data=train_data_frame_clean[,-(1:4)],mincut=1,mindev=1e-2)
plot(train_tree, col=8, lwd=2,type="uniform")
text(train_tree, label="yprob")

train_rf <- randomForest(as.factor(win) ~ ., data=train_data_frame_clean[,-(1:4)], 
                         ntree=1000, nodesize=1, importance=TRUE, mindev=1e-5)
varImpPlot(train_rf,  type=1, pch=21, bg="navy", main='RF variable importance')


#### Making Predictions ####
predictions_rpart <- predict(train_rpart,newdata=test_data_frame,type="class")
test_data_frame$Win <- predictions_rpart
submission <- test_data_frame[,1:2]

#### Lasso ####

simple_train <- train_data_frame_clean[,-(1:4)]
xtrain <- sparse.model.matrix(~., data=simple_train)[,-1]
ytrain <- train_data_frame_clean$Win
train_lasso <- cv.gamlr(xtrain,ytrain,family="binomial")

#### Simple Binomial ####
simple_train <- train_data_frame[,-(1:4)]
simple_train$Win <- train_data_frame$Win
train_binomial <- glm(Win ~ .,data=simple_train,family="binomial")

#### Create Test Data Set ####
test_data_frame <- data.frame()
for (i in 2012:2015) {
  test_data_frame <- rbind(test_data_frame,test_frame_model(i))
  print(i)
}
test_data_frame_clean <- test_data_frame[,colnames(train_data_frame_clean)]

simple_test <- test_data_frame_clean[,-(1:4)]
xtest <- sparse.model.matrix(~., data=simple_test)[,-1]
#### Compare to Actuals ####
evaluation(prediction_model=train_rf,response="class",test_seasons=2012:2015)

#### Prediction File ####
submission <- data.frame()
for(i in 2012:2015) {
  submission <- rbind(submission,submissionFile(i))
}
submission$Win <- drop(predict(train_rf,newdata=simple_test,type="prob"))
write.csv(submission,file="output/submission2015_R1_2016-03-05.csv",row.names=FALSE)

#### Sample Testing ####
full_data <- data.frame()
for(i in 2003:2015) {
  full_data <- rbind(full_data,train_frame_model(i))
  print(i)
}
full_data_clean <- full_data[,colSums(is.na(full_data)) == 0]

PredAccuracy <- list(CART=NULL, RF=NULL)
n <- 40
N <- nrow(full_data_clean) - n
y <- as.factor(full_data_clean[,2])
x <- full_data_clean[,-(1:4)]
for(i in 1:50){
  train <- sample(1:nrow(full_data_clean), N) # should actually split things in to folds
  
  rlasso <- cv.gamlr(x[train],y[train],family="binomial")
  yhat.rlasso <- predict(rlasso, newdata=x[-train,],type="response")
  yhat.rlasso <- rhat.rlasso>.5
  PredCorrect.rlasso <- (y[-train]==yhat.rlasso)*1
  PredAccuracy$Lasso <- c( PredAccuracy$Lasso, mean(PredCorrect.rlasso))
  
  rt <- tree(y[train] ~ ., data=x[train,],mincut=2,mindev=1e-5)
  yhat.rt <- predict(rt, newdata=x[-train,])
  PredCorrect.rt <- (y[-train]==yhat.rt)*1
  PredAccuracy$CART <- c( PredAccuracy$CART, mean(PredCorrect.rt))
  
  rf <- randomForest(y[train] ~ ., data=x[train,], ntree=1000, nodesize=2)
  yhat.rf <- predict(rf, newdata=x[-train,])
  PredCorrect.rf <- (y[-train]==yhat.rf)*1
  PredAccuracy$RF <- c( PredAccuracy$RF, mean(PredCorrect.rf))
  
  cat(i)
} 
par(mfrow=c(1,2))
boxplot(PredAccuracy, col="dodgerblue", xlab="model", ylab="Accuracy Prediction",
        main="Accuracy of Models")
abline(a=.5,b=0,lty=2)
hist(PredAccuracy$RF,main="Histogram of Random Forest Accuracy",xlab="RF Accuracy Prediction",
     border="white",col="grey")
mean(PredAccuracy$RF)
median(PredAccuracy$RF)

