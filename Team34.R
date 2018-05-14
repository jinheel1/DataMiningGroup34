# Group 34: Maria Rodriguez (mar1); Anirudh Narain (anarain); Kayla Oliva (keoliva); Jinhee Lee (jinheel1)

## Part 1. Unsupervised Learning
# can still assume `df` has attributes `trainX` and `trainY`
load("data/TrainData.RData")
# sample usage:
#   clusterLabel1 = unsupModel1(rbind(trainX, testX))
#   clusterLabel2 = unsupModel2(rbind(trainX, testX))
unsupModel1 <- function(X) {
  pc.n = prcomp(X, center = T, scale = T)
  pc.65 = pc.n$x[,1:65]
  kmeans.results.pca = kmeans(pc.65, centers = 2, nstart = 25, iter.max = 50, algorithm = "Lloyd")
  return(kmeans.results.pca$cluster)
}

unsupModel2 <- unsupModel1

## Part 2. Supervised Learning
# TODO: train supModel1 with training data here
data <- as.data.frame(cbind(trainX, trainY))
mode = randomForest(as.factor(trainY) ~ ., trainX, mtry = 50, importance = T)


# TODO: train supModel2 with training data here
pc.n <- prcomp(trainX, center=TRUE, scale=TRUE)
pc.12 = pc.n$x[,1:12]
lda.fit <- lda(grouping = trainY, x = pc.12)

# sample usage:
#   classLabel1 = supModel1(testX)
#   classLabel2 = supModel2(testX)
# install.packages("randomForest")

library(randomForest)

supModel2 <- function(testX) {
  predict(mode, testX)
}

supModel1 <- supModel2
