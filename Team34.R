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

# TODO: train supModel2 with training data here

# sample usage:
#   classLabel1 = supModel1(testX)
#   classLabel2 = supModel2(testX)
supModel1 <- function(testX) {
  
}

supModel2 <- supModel1
