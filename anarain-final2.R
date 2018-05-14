load("TrainData.Rdata")
trainY.df = data.frame(trainY)

require(MASS)

pr.out <- prcomp(trainX, scale=TRUE, center = TRUE)

Cols <- function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# plots of scores of first 3 principal components. 
# Even from these we can see clear differentiation of the data according to class labels 
# (even though they were not used in any calculation)
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col = Cols(trainY), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col=Cols(trainY), pch = 19, xlab = "Z1", ylab = "Z2")

# plotting proportion of variance explained (PVE) for each PC, and cumulative PVE
pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(pve, type = "o", ylab = "PVE", xlab = "Principal component")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal component")
plot(diff(pve))

# There's a somewhat elbow point after the ~12th principal component, 
# but up to 12 principal components only explains about 51% of the variance, 
# which isn't too much. But adding more components doesn't add much to PVE.


# LDA using full dataset
require(MASS)
trainX.mat = as.matrix(trainX)
lda.fit <- lda(grouping = trainY, x = trainX.mat)
lda.pred <- predict(lda.fit, trainX)
table(lda.pred$class, trainY)

# creating a matrix to store error values to compare full dataset to PCA's data
reps = 50
lda.test.errs <- matrix(NA, 3, reps)

# the testing
for (i in 1:reps) {
  samp <- sample(1:length(trainY), ceiling(length(trainY)/5))
  train.lda <- trainX[-samp,]
  test.lda <- trainX[samp,]
  lda.fit.1 <- lda(grouping = trainY[-samp], x = train.lda)
  lda.fit.pred.1 <- predict(lda.fit.1, newdata = test.lda)
  lda.test.errs[1,i] = sum(trainY[samp] != lda.fit.pred.1$class) / length(trainY[samp])
}

# LDA using PCA'd dataset (PC = 65)

trainX.pc65 <- pr.out$x[,1:65]

lda.fit.pc65 <- lda(grouping= trainY, x = trainX.pc65)
lda.fit.pc65.pred <- predict(lda.fit.pc65, trainX.pc65)
table(lda.fit.pc65.pred$class, trainY)

# testing
for (i in 1:reps) {
  samp <- sample(1:length(trainY), ceiling(length(trainY)/5))
  train.lda.pc65 <- trainX.pc65[-samp,]
  test.lda.pc65 <- trainX.pc65[samp,]
  lda.fit.pc65.1 <- lda(grouping = trainY[-samp], x = train.lda.pc65)
  lda.fit.pc65.pred.1 <- predict(lda.fit.pc65.1, newdata = test.lda.pc65)
  lda.test.errs[2,i] = sum(trainY[samp] != lda.fit.pc65.pred.1$class) / length(trainY[samp])
}


# LDA using PCA'd dataset (PC = 12)

trainX.pc12 <- pr.out$x[,1:12]

lda.fit.pc12 <- lda(grouping= trainY, x = trainX.pc12)
lda.fit.pc12.pred <- predict(lda.fit.pc12, trainX.pc12)
table(lda.fit.pc12.pred$class, trainY)

# testing
for (i in 1:reps) {
  samp <- sample(1:length(trainY), ceiling(length(trainY)/5))
  train.lda.pc12 <- trainX.pc12[-samp,]
  test.lda.pc12 <- trainX.pc12[samp,]
  lda.fit.pc12.1 <- lda(grouping = trainY[-samp], x = train.lda.pc12)
  lda.fit.pc12.pred.1 <- predict(lda.fit.pc12.1, newdata = test.lda.pc12)
  lda.test.errs[3,i] = sum(trainY[samp] != lda.fit.pc12.pred.1$class) / length(trainY[samp])
}

# This command gives the average test error for all three data (original dataset, 
# 65 PC's, and 12 PC's, respectively)
rowMeans(lda.test.errs)


# logistic regression using full dataset
alldata <- cbind(trainX, trainY)
log.fit <- glm(trainY ~ ., data = alldata, family = binomial)
log.probs <- predict(log.fit, type = "response")
log.pred <- rep(0, 133)
log.pred[log.probs > 0.5] = 1
table(log.pred, trainY)

# creating a matrix to store error values to compare full dataset to PCA's data
reps = 50
log.test.errs <- matrix(NA, 3, reps)

# the testing
for (i in 1:reps) {
  samp <- sample(1:length(trainY), ceiling(length(trainY)/5))
  train.log <- alldata[-samp,]
  test.log <- alldata[samp,]
  log.fit.1 <- glm(trainY~., data = train.log, family = binomial)
  log.probs.1 <- predict(log.fit.1, newdata = test.log, type = "response")
  log.pred.1 <- rep(0, length(samp))
  log.pred.1[log.probs.1 > 0.5] = 1
  log.test.errs[1,i] = sum(log.pred.1 != test.log$trainY) / length(samp)
}

# logistic regression using PCA'd dataset (PC = 65)

alldata.pc65 <- cbind(trainX.pc65, trainY.df)
log.fit.pc65 <- glm(trainY ~ ., data = alldata.pc65, family = binomial)
log.probs.pc65 <- predict(log.fit.pc65, type = "response")
log.pred.pc65 <- rep(0, 133)
log.pred.pc65[log.probs.pc65 > 0.5] = 1
table(log.pred.pc65, trainY)

# testing
for (i in 1:reps) {
  samp <- sample(1:length(trainY), ceiling(length(trainY)/5))
  train.log.pc65 <- alldata.pc65[-samp,]
  test.log.pc65 <- alldata.pc65[samp,]
  log.fit.1.pc65 <- glm(trainY~., data = train.log.pc65, family = binomial)
  log.probs.1.pc65 <- predict(log.fit.1.pc65, newdata = test.log.pc65, type = "response")
  log.pred.1.pc65 <- rep(0, length(samp))
  log.pred.1.pc65[log.probs.1.pc65 > 0.5] = 1
  log.test.errs[2,i] = sum(log.pred.1.pc65 != test.log.pc65$trainY) / length(samp)
}


# logistic regression using PCA'd dataset (PC = 65)

alldata.pc12 <- cbind(trainX.pc12, trainY.df)
log.fit.pc12 <- glm(trainY ~ ., data = alldata.pc12, family = binomial)
log.probs.pc12 <- predict(log.fit.pc12, type = "response")
log.pred.pc12 <- rep(0, 133)
log.pred.pc12[log.probs.pc12 > 0.5] = 1
table(log.pred.pc12, trainY)

# testing
for (i in 1:reps) {
  samp <- sample(1:length(trainY), ceiling(length(trainY)/5))
  train.log.pc12 <- alldata.pc12[-samp,]
  test.log.pc12 <- alldata.pc12[samp,]
  log.fit.1.pc12 <- glm(trainY~., data = train.log.pc12, family = binomial)
  log.probs.1.pc12 <- predict(log.fit.1.pc12, newdata = test.log.pc12, type = "response")
  log.pred.1.pc12 <- rep(0, length(samp))
  log.pred.1.pc12[log.probs.1.pc12 > 0.5] = 1
  log.test.errs[3,i] = sum(log.pred.1.pc12 != test.log.pc12$trainY) / length(samp)
}

# This command gives the average test error for all three data (original dataset, 
# 65 PC's, and 12 PC's, respectively)
rowMeans(log.test.errs)


# K-nn regression
library(class)
samp <- sample(1:133, 33)
train.knn <- trainX[-samp,]
test.knn <- trainX[samp,]

knn.pred <- knn(train.knn, test.knn, trainY[-samp], k = 7)

table(knn.pred, trainY[samp])

sum(knn.pred != trainY[samp])/length(trainY[samp])