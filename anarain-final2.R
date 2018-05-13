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


# LDA
trainX.mat = as.matrix(trainX)

# Fitting LDA to the data
lda.fit <- lda(grouping = trainY, x = trainX.mat)

# prior prob of group 0: 0.8346; prior prob of group 1: 0.1654
plot(lda.fit)

lda.pred <- predict(lda.fit, trainX)
table(lda.pred$class, trainY)


# testing
set.seed(1)
samp <- sample(1:133, 33)
train.lda <- trainX.mat[-samp,]
test.lda <- trainX[samp,]
lda.fit.1 <- lda(grouping = trainY[-samp], x = train.lda)

lda.predzz <- predict(lda.fit.1, newdata = test.lda)
table(lda.predzz$class, trainY[samp])

# Misclassification error:
sum(lda.predzz$class != trainY[samp])/length(trainY[samp])


# logistic regression

alldata <- cbind(trainX, trainY)
log.fit <- glm(trainY ~ ., data = alldata, family = binomial)
log.probs <- predict(log.fit, type = "response")
log.pred <- rep(0, 133)
log.pred[log.probs > 0.5] = 1

table(log.pred, trainY)

# testing
set.seed(1)
samp <- sample(1:133, 33)
train.alldata <- alldata[-samp,]
test.alldata <- alldata[samp,]
log.fit.1 <- glm(trainY~ ., data= train.alldata, family=binomial)
log.prob.1 <- predict(log.fit.1, newdata = test.alldata, type = "response")
log.predzz <- rep(0,33)
log.predzz[log.prob.1 > 0.5] = 1
table(log.predzz, test.alldata$trainY)

# Misclassification error:
sum(log.predzz != test.alldata$trainY)/length(test.alldata$trainY)

# K-nn regression
library(class)
samp <- sample(1:133, 33)
train.knn <- trainX[-samp,]
test.knn <- trainX[samp,]

knn.pred <- knn(train.knn, test.knn, trainY[-samp], k = 7)

table(knn.pred, trainY[samp])

sum(knn.pred != trainY[samp])/length(trainY[samp])