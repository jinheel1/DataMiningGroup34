# Group 34: Maria Rodriguez (mar1); Anirudh Narain (anarain); Kayla Oliva (keoliva); Jinhee Lee (jinheel1)

# trainX <- read.table(file="trainX.txt",header=TRUE)
# trainY <- read.table(file="trainy.txt",header=TRUE)
load("TrainData.RData")
trainY.df = data.frame(trainY)



# Anirudh

pr.out <- prcomp(trainX, scale=TRUE)
summary(pr.out)

pca.test <- pr.out$rotation
dim(pr.out$x)

biplot(pr.out, scale = 0)



# Anirudh

require(MASS)

n = nrow(trainX)
p = ncol(trainX)

a = lda(trainX, trainY)
at = a$scaling

trainX.mat = as.matrix(trainX)

z = trainX.mat %*% at

plot(z, col = trainY+1)



# Jin (K-Medoids Clustering and Visualization)

library(factoextra)
library(cluster)
fviz_nbclust(trainX, pam, method = "silhouette") + theme_classic()


pam.res = pam(trainX, k = 2)

fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
             )



# Kayla

library(combinat)

best_cluster <- function(cut1, cut2) {
  per <- permn(1:2)
  hamming <- numeric(length(per))
  for (i in 1:length(per)) {
    ts <- table(cut1, cut2)
    ts <- ts[per[[i]],]
    diag(ts) <- 0
    hamming[i] <- sum(ts)
  }
  return(c(min(hamming), which.min(hamming)))
}



# Jin (K-Medoids Hamming Clustering error)

library(ClusterR)

best_cluster(trainY, pam.res$clustering)

###best_cluster(trainY, pam.res$clustering) returns "[1] 14  1". So the error is 14/133 = .1052632. Roughly 89.5% accuracy.




# Anirudh (sent to Jin for knn Supervised Learning)

### K-nn regression
library(class)

samp <- sample(1:133, 33)
train.knn <- trainX[-samp,]
test.knn <- trainX[samp,]

knn.pred <- knn(train.knn, test.knn, trainY[-samp], k = 7)

table(knn.pred, trainY[samp])

min(sum(knn.pred != trainY[samp])/length(trainY[samp]), 
                   sum(!knn.pred != trainY[samp])/length(trainY[samp]))




# Kayla

pc.n = prcomp(X, center = T, scale = T)
dirs = pc.n$rotation
scrs = pc.n$x
dim(scrs)
pc.n$x[,1:92]
summary(pc.n)



