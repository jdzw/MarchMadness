setwd("~/Personal/MarchMadness_2015")
source("2015_utility_v2.R")
library(tree)
library(randomForest)
library(gamlr)

regSeason <- read.csv("inputs/Stage2/RegularSeasonDetailedResults.csv",header=TRUE,stringsAsFactors = FALSE)
seasons <- read.csv("inputs/Stage2/Seasons.csv",header=TRUE,stringsAsFactors = FALSE)
teams <- read.csv("inputs/Stage2/Teams.csv",header=TRUE,stringsAsFactors = FALSE)
tourneyRes <- read.csv("inputs/Stage2/TourneyCompactResults.csv",header=TRUE,stringsAsFactors = FALSE)
tourneySeeds <- read.csv("inputs/Stage2/TourneySeeds.csv",header=TRUE,stringsAsFactors = FALSE)
tourneySlots <- read.csv("inputs/Stage2/TourneySlots.csv",header=TRUE,stringsAsFactors = FALSE)
ratings1 <- read.csv("inputs/massey_ordinals_2003-2015.csv")
ratings2 <- read.csv("inputs/Stage2/MasseyOrdinals2016ThruDay128_63systems.csv")
ratings <- rbind(ratings1,ratings2)
ratings <- reshape(ratings,timevar="sys_name",idvar=c("season","rating_day_num","team"),direction="wide")


#### Train Data Frame ####
train_data_frame <- data.frame()
for(i in 2003:2015) {
  train_data_frame <- rbind(train_data_frame,train_frame_model(i))
  print(i)
}
ratings_data <- train_data_frame[,c(1:4,7,166,13:152,171,330,177:316)]
ratings_data$A_AvgRank <- rowMeans(ratings_data[,7:146],na.rm=TRUE)
ratings_data$B_AvgRank <- rowMeans(ratings_data[,149:288],na.rm=TRUE)
ratings_data_clean <- ratings_data[,colSums(is.na(ratings_data)) == 0]
y <- as.factor(ratings_data_clean[,2])
x <- ratings_data_clean[,-(1:4)]
xx <- sparse.model.matrix(~.^2, data=x)[,-1]

#### Models ####
rcv <- cv.gamlr(x=xx,y=y,family="binomial")
plot(rcv)
plot(rcv$gamlr)
yhat <- predict(rcv,newdata=xx,type="response",select="min")
yhat <- (yhat > 0.5)*1
plot(y,yhat)
mean((y==yhat)*1)

rf <- randomForest(y ~ ., data=x, ntree=5000, nodesize=1,importance=TRUE)
varImpPlot(rf,  type=1, pch=21, bg="navy", main='RF variable importance')
yhat.rf <- predict(rf, newdata=x)
mean((y==yhat.rf)*1)

#### Test Data Frame ####
test_data_frame <- test_frame_model(2016)
test_data <- test_data_frame[,c(1:4,7,166,13:152,171,330,177:316)]
test_data$A_AvgRank <- rowMeans(test_data[,7:146],na.rm=TRUE)
test_data$B_AvgRank <- rowMeans(test_data[,149:288],na.rm=TRUE)
test_data_clean <- test_data[,colSums(is.na(ratings_data)) == 0]
xtest <- test_data_clean[,-(1:4)]
xxtest <- sparse.model.matrix(~.^2, data=xtest)[,-1]

#### Submission File ####
submission <- submissionFile(2016)
submission$Pred <- predict(rf,newdata=xtest,type="prob")[,2]
write.csv(submission,file="output/submission2015_R2_2016-03-14_RF.csv",row.names=FALSE)

submission$Pred <- predict(rcv,newdata=xxtest,type="response",select="min")
write.csv(submission,file="output/submission2015_R2_2016-03-14_CV.csv",row.names=FALSE)

#### Deviance ####
actuals <- data.frame()
for (i in 2012:2015) {
  actuals <- rbind(actuals,train_frame_model(i))
  print(i)
}
actuals_data <- actuals[,c(1:4,7,158,13:144,163,314,169:300)]
actuals_data$A_AvgRank <- rowMeans(actuals_data[,7:138],na.rm=TRUE)
actuals_data$B_AvgRank <- rowMeans(actuals_data[,141:272],na.rm=TRUE)
actuals_data_clean <- actuals_data[,colSums(is.na(ratings_data)) == 0]
yactuals <- as.factor(actuals_data_clean[,2])
xactuals <- actuals_data_clean[,-(1:4)]
xxactuals <- sparse.model.matrix(~.^2, data=xactuals)[,-1]

yhat.cv <- predict(rcv, newdata=xxactuals,type="response",select="min")
logloss(nrow(xxactuals),yhat.cv,as.numeric(yactuals))

yhat.rf <- predict(rf, newdata=xactuals,type="prob")[,2]
logloss(nrow(xactuals),yhat.rf,as.numeric(yactuals))
