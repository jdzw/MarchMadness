setwd("~/Personal/MarchMadnessPredictions")

regseason <- read.csv("inputs/RegularSeasonDetailedResults.csv")
regseason$Wfgp <- regseason$Wfgm/regseason$Wfga
regseason$Wfgp3 <- regseason$Wfgm3/regseason$Wfga3
regseason$Lfgp <- regseason$Lfgm/regseason$Lfga
regseason$Lfgp3 <- regseason$Lfgm3/regseason$Lfga3
regseason$Wper3 <- regseason$Wfgm3*3/regseason$Wscore
regseason$Lper3 <- regseason$Lfgm3*3/regseason$Lscore

winner <- regseason[,-c(2,5:6,22:34,37,38,40)]
winner$outcome <- 1
colnames(winner) <- c("Season","team","score","loc","Numot","fgm","fga","fgm3","fga3","ftm","fta","or",   
                       "dr","ast","to","stl","blk","pf","fgp","fgp3","per3","outcome")
loser <- regseason[,c(1,5:8,22:34,37:38,40)]
loser$outcome <- 0
colnames(loser) <- c("Season","team","score","loc","Numot","fgm","fga","fgm3","fga3","ftm","fta","or",   
                      "dr","ast","to","stl","blk","pf","fgp","fgp3","per3","outcome")
results <- rbind(winner,loser)
xresults <- scale(results[,5:21])

# K-means fitting
source("utility/kIC.R")
kfit <- lapply(c(1:40), function(k) kmeans(xresults,k))
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")
plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(0,kaicc,kbic)),
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
print(which.min(kbic))

kmns <- kmeans(xresults,2)
tapply(results$outcome,kmns$cluster,table)

# PCA
pcax <- prcomp(results[,5:21], scale=TRUE)
plot(pcax, main="")
mtext(side=1, "College Basketball Team Principle Components",  line=1, font=2)

teampc <- predict(pcax)
plot(teampc[,1:2], pch=21, bg=(4:2)[results$outcome], main="")



### marginal regression
xteam <- as.matrix(results[,5:21])
win <- results$outcome
win <- factor(win)
phi <- cor(xteam, results$outcome)/apply(xteam,2,sd) 
z <- xteam%*%phi
fwd <- glm(results$outcome ~ z, family="binomial")
plot(win,fwd$fit, pch=21, bg="lightgreen", ylab="marginal regression fit")
