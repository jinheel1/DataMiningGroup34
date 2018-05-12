# Group 34: Maria Rodriguez (mar1); Anirudh Narain (anarain); Kayla Oliva (keoliva); Jinhee Lee (jinheel1)
setwd("~/_CMU/_S18/36462_DataMining/final/DataMiningGroup34")

# Problem 2 (85 points)

X <- read.table(file="trainX.txt",header=TRUE)

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

pc = prcomp(X)
dirs = pc$rotation
scrs = pc$x
summary(pc)
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
#y <- read.table(file="trainy.txt",header=TRUE)


km = kmeans(X, centers = 3, nstart = 10, algorithm = "Lloyd")


## Part 2. Supervised Learning

