# Group 34: Maria Rodriguez (mar1); Anirudh Narain (anarain); Kayla Oliva (keoliva); Jinhee Lee (jinheel1)
setwd("~/_CMU/_S18/36462_DataMining/final/DataMiningGroup34")

# Problem 2 (85 points)

X <- read.table(file="trainX.txt",header=TRUE)
y <- read.table(file="trainy.txt",header=TRUE)

## Part 1. Unsupervised Learning

### 1a.

#Starting off I just want to understand the data to get a handle on it.
install.packages("corrplot")
library(corrplot)

dim(X)
#summary(X)

first = X[,1:100]

#create correlation matrix

cor_jnk=cor(first, use="complete.obs")
#plot cor matrix
corr1 = corrplot(cor_jnk, order="AOE", method="color", type="upper",
         is.corr = T, addCoefasPercent = F,
         p.mat = 1-abs(cor_jnk), sig.level=0.90, insig = "blank")
#There seems to be a small number of variables with high correlations.
#But I haven't reduced the dimensions so we can check correlations after removing variables.

pc = prcomp(X, center = T)
summary(pc)
#PC1 explains 57.5% of the data.
#PC12 explains 90.5%
#PC22 explains 95%
#PC46 explains 99%
screeplot(pc, npcs = 20, type = "lines")

pc.n = prcomp(X, center = T, scale = T)
dirs = pc.n$rotation
scrs = pc.n$x
dim(scrs)
pc.n$x[,1:92]
summary(pc.n)
screeplot(pc, npcs = 20, type = "lines")

install.packages("factoextra")
library(factoextra)
fviz_pca_ind(pc.n,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

matrix(pc.n)
fviz_pca_var(pc.n,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pc.n, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

##LDA results in 3 different groups.
require(MASS)

n = nrow(trainX)
p = ncol(trainX)

a = lda(trainX, trainY)
at = a$scaling

trainX.mat = as.matrix(trainX)

z = trainX.mat %*% at

plot( z, col = trainY+1)

### 1b.
y <- read.table(file="trainy.txt",header=TRUE)


kmeans.results = kmeans(X, centers = 3, nstart = 10, iter.max = 20, algorithm = "Lloyd")
kmeans.results$cluster

#HW1
# Permute.label maps 1: length(permuted.values) into a permuted value
# Returns an integer
permute.label <- function(X_c, permuted.values){ return(permuted.values[x])
}

# Load Combinat package, which contains permn, #a function that generates all permations
library(combinat)
sampled.rows <- sample(1:nrow(X),133)
random.centers <- X[sampled.rows,]

kmeans.wcv <- numeric(4)
kmeans.clusters <- matrix(0, nrow= nrow(X), ncol = 4)
initial.centers <- list()
for(ii in 1:4) {
  #Run K-means, with centers given by random.centers
  initial.centers[[ii]] = random.centers[3*(ii-1)+1:3,]
  kmeans.results= kmeans(X, centers =initial.centers[[ii]], iter.max = 20, algorithm = "Lloyd")
  # Store WCV and cluster assignments
  kmeans.wcv[ii] = kmeans.results$tot.withinss
  kmeans.clusters[,ii] = kmeans.results$cluster
  #permn from combinat package generates permutations
  permutations = permn(1:3)
  # Permute cluster assignments to find best match.
  permuted.clusters = matrix(nrow=nrow(X), ncol = length(permutations)) # Hamming vector will be used to find best permutation
  hamming.vector = numeric(length(permutations))
  # Loop over jj, the possible permutations of labels
  for( jj in 1: length(permutations) ) {
    permuted.clusters[,jj] <- sapply(X=kmeans.clusters[,ii],
                                     FUN = permute.label,
                                     permuted.values=permutations[[jj]])
    hamming.vector[jj] <- sum(y != permuted.clusters[,jj])
  }
  # Find permutation that minimizes hamming distance
  min.permute <- which.min(hamming.vector)

  kmeans.clusters[,ii] <- permuted.clusters[,min.permute]
}
# Report relevant output
initial.centers

#for (ii in 1: 4){ print(table(kmeans.clusters[,ii],y))}

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

length(kmeans.results$cluster)
length(y$label + 1)
best_cluster(kmeans.results$cluster, y$label + 1)
1 - 18/133

## Part 2. Supervised Learning

