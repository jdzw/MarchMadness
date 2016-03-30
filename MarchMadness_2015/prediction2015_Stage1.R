#### Train Data Frame ####
train_data_frame <- data.frame()
for(i in 2003:2011) {
  train_data_frame <- rbind(train_data_frame,train_frame_model(i))
  print(i)
}
ratings_data <- train_data_frame[,c(1:4,7,158,13:144,163,314,169:300)]
ratings_data$A_AvgRank <- rowMeans(ratings_data[,7:138],na.rm=TRUE)
ratings_data$B_AvgRank <- rowMeans(ratings_data[,141:272],na.rm=TRUE)
ratings_data_clean <- ratings_data[,colSums(is.na(ratings_data)) == 0]
y <- as.factor(ratings_data_clean[,2])
x <- ratings_data_clean[,-(1:4)]
xx <- sparse.model.matrix(~.^2, data=x)[,-1]

#### Models ####
rcv <- cv.gamlr(x=xx,y=y,family="binomial")
rf <- randomForest(y ~ ., data=x, ntree=5000, nodesize=1)

#### Test Data Frame ####
test_data_frame <- data.frame()
for (i in 2012:2015) {
  test_data_frame <- rbind(test_data_frame,test_frame_model(i))
  print(i)
}
test_data <- test_data_frame[,c(1:4,7,158,13:144,163,314,169:300)]
test_data$A_AvgRank <- rowMeans(test_data[,7:138],na.rm=TRUE)
test_data$B_AvgRank <- rowMeans(test_data[,141:272],na.rm=TRUE)
test_data_clean <- test_data[,colSums(is.na(test_data)) == 0]
xtest <- test_data_clean[,-(1:4)]
xxtest <- sparse.model.matrix(~.^2, data=xtest)[,-1]

#### Submission File ####
submission <- data.frame()
for(i in 2012:2015) {
  submission <- rbind(submission,submissionFile(i))
}
submission$Pred <- predict(rf,newdata=xtest,type="prob")[,2]
write.csv(submission,file="output/submission2015_R1_2016-03-13_RF.csv",row.names=FALSE)

submission$Pred <- predict(rcv,newdata=xxtest,type="response",select="min")
write.csv(submission,file="output/submission2015_R1_2016-03-13_CV.csv",row.names=FALSE)

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
