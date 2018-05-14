# this code is for testing the teamx.R file
source("Team34.R")

suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(pdfCluster))

# In your code, you should assume all the data files are in
# folder 'data/', it is recommended to read in the training 
# data by doing load("data/TrainData.RData"). Your code should 
# produce four functions:
#
#    unsupModel1 and unsupModel2 takes in a data frame and
#    outputs a vector of cluster labels for each row of the
#    data frame
#
#    supModel1 and supModel2 takes in a data frame and outputs
#    the prediction for class labels for each row of the data
#    frame

load("data/TestData.RData")

if (class(trainY) != "numeric"){
  trainY = trainY$label
}

assert_that(length(trainY) == 106)
assert_that(nrow(trainX) == 106)

clusterLabel1 = unsupModel1(rbind(trainX, testX))
clusterLabel2 = unsupModel2(rbind(trainX, testX))

assert_that(length(clusterLabel1) == nrow(testX) + nrow(trainX))
assert_that(length(clusterLabel2) == nrow(testX) + nrow(trainX))

classLabel1 = supModel1(testX)
classLabel2 = supModel2(testX)

assert_that(length(classLabel1) == nrow(testX))
assert_that(length(classLabel2) == nrow(testX))

unsupErrorRate1 = adj.rand.index(clusterLabel1, c(trainY, testY))
unsupErrorRate2 = adj.rand.index(clusterLabel2, c(trainY, testY))

supErrorRate1 = mean(testY != classLabel1)
supErrorRate2 = mean(testY != classLabel2)

cat("PS: rand index is better if it's closer to 1. \n")
cat(paste("Rand index for unsupervised model 1: ", as.character(round(unsupErrorRate1, 4)), '\n'))
cat(paste("Rand index for unsupervised model 2: ", as.character(round(unsupErrorRate2, 4)), '\n'))
cat(paste("Error rate for supervised model 1: ", as.character(round(supErrorRate1 * 100, 4)), '%\n'))
cat(paste("Error rate for supervised model 2: ", as.character(round(supErrorRate2 * 100, 4)), '%\n'))