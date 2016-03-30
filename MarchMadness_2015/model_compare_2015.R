# Comparison of Prediction Models

setwd("~/Personal/MarchMadness_2015")
source("2015_utility_v2.R")
library(tree)
library(randomForest)
library(gamlr)

regSeason <- read.csv("inputs/RegularSeasonDetailedResults.csv",header=TRUE,stringsAsFactors = FALSE)
seasons <- read.csv("inputs/Seasons.csv",header=TRUE,stringsAsFactors = FALSE)
teams <- read.csv("inputs/Teams.csv",header=TRUE,stringsAsFactors = FALSE)
tourneyRes <- read.csv("inputs/TourneyCompactResults.csv",header=TRUE,stringsAsFactors = FALSE)
tourneySeeds <- read.csv("inputs/TourneySeeds.csv",header=TRUE,stringsAsFactors = FALSE)
tourneySlots <- read.csv("inputs/TourneySlots.csv",header=TRUE,stringsAsFactors = FALSE)
ratings <- read.csv("inputs/massey_ordinals_2003-2015.csv")
ratings <- reshape(ratings,timevar="sys_name",idvar=c("season","rating_day_num","team"),direction="wide")

full_data <- data.frame()
for(i in 2003:2015) {
  full_data <- rbind(full_data,train_frame_model(i))
  print(i)
}
full_data_clean <- full_data[,colSums(is.na(full_data)) == 0]

ratings_data <- full_data[,c(1:4,7,158,13:144,163,314,169:300)]
ratings_data$A_AvgRank <- rowMeans(ratings_data[,7:138],na.rm=TRUE)
ratings_data$B_AvgRank <- rowMeans(ratings_data[,141:272],na.rm=TRUE)
ratings_data_clean <- ratings_data[,colSums(is.na(ratings_data)) == 0]

model.compare <- function(xdata,ydata,testsize,iter) {
  PredAccuracy <- list(CV=NULL,CART=NULL, RF=NULL)
  PredScore <- list(CV=NULL,CART=NULL,RF=NULL)
  N <- nrow(xdata) - testsize
  for(i in 1:iter){
    train <- sample(1:nrow(xdata), N) # should actually split things in to folds
    
    xdata2 <- sparse.model.matrix(~.^2, data=xdata)[,-1]
    rcv <- cv.gamlr(x=xdata2[train,],y=ydata[train],family="binomial")
    yhat.cv <- predict(rcv, newdata=xdata2[-train,],type="response",select="min")
    yhat.cv <- (yhat.cv > 0.5)*1
    PredCorrect.cv <- (ydata[-train]==yhat.cv)*1
    PredAccuracy$CV <- c( PredAccuracy$CV, mean(PredCorrect.cv))
    yhat.cv.2 <- predict(rcv, newdata=xdata2[-train,],type="response",select="min")
    PredScore$CV <- c(PredScore$CV,logloss(n,yhat.cv.2,as.numeric(ydata[-train])))
  
    rt <- tree(ydata[train] ~ ., data=xdata[train,],mincut=1,mindev=1e-10)
    yhat.rt <- predict(rt, newdata=xdata[-train,],type="class")
    PredCorrect.rt <- (ydata[-train]==yhat.rt)*1
    PredAccuracy$CART <- c( PredAccuracy$CART, mean(PredCorrect.rt))
    yhat.rt.2 <- predict(rt, newdata=xdata[-train,],type="vector")
    yhat.rt.2 <- yhat.rt.2[,2]
    PredScore$CART <- c(PredScore$CART,logloss(n,yhat.rt.2,as.numeric(ydata[-train])))
  
    rf <- randomForest(ydata[train] ~ ., data=xdata[train,], ntree=1000, nodesize=1)
    yhat.rf <- predict(rf, newdata=xdata[-train,])
    PredCorrect.rf <- (ydata[-train]==yhat.rf)*1
    PredAccuracy$RF <- c( PredAccuracy$RF, mean(PredCorrect.rf))
    yhat.rf.2 <- predict(rf, newdata=xdata[-train,],type="prob")
    yhat.rf.2 <- yhat.rf.2[,2]
    PredScore$RF <- c(PredScore$RF,logloss(n,yhat.rf.2,as.numeric(ydata[-train])))
  
    cat(i)
  } 
  par(mfrow=c(1,2))
  boxplot(PredAccuracy, col="dodgerblue", xlab="model", ylab="Accuracy Prediction",
        main="Accuracy")
  abline(a=.5,b=0,lty=2)
  boxplot(PredScore, col="dodgerblue", xlab="model", ylab="Log-Loss Score",
        main="Deviance")
}

y <- as.factor(ratings_data_clean[,2])
x <- ratings_data_clean[,-(1:4)]
n <- 40
z <- model.compare(x,y,n,25)
