library(L1LinearModel)
library(testthat)
context("../../R/LinearModelL1.R")


# set up for regression
test_that("Test the ability to use testthat functions",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  penalty <- 2
  opt.thresh <- .000007
  step.size <- .1
  Labels <- ozone[,1] # first col is output of the data

  Data <- ozone[,2:ncol(ozone)]
  
  
  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]

  X.scaled.mat <- scale(TrainingData)
  initial.weight.vec <- rep(0.0,ncol(X.scaled.mat) + 1)
  output <- LinearModelL1(X.scaled.mat, TrainingLabels, penalty, opt.thresh, initial.weight.vec, step.size)

  show(output)
  succeed()
})


# set up for binary classification
test_that(
  "NN1toKmaxPredict handles wrong max.neighbors sizes",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x <- zip.train[train.i,-1]
    y <- zip.train[train.i, 1]
    testx <- zip.train[test.i, -1]
    max.neighbors <- 3000
    pred.vec <- NN1toKmaxPredict(x,y,testx,max.neighbors)
    expect_false(is.null(pred.vec))
  })
