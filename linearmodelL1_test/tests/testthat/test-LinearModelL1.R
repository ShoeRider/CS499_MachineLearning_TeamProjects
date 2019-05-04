library(L1LinearModel)
library(testthat)
context("../../R/LinearModelL1.R")


# set up for regression
test_that("Tests to see if the dimensions will be checked",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  penalty <- 2
  output <- NULL
  opt.thresh <- .0000007
  step.size <- 2
  
  Labels <- ozone[,1] # first col is output of the data

  Data <- ozone[,2:ncol(ozone)]
  
  TrainingData <- as.matrix(Data)[1:57,]

  TrainingLabels <- Labels[1:54]

  X.scaled.mat <- scale(TrainingData)
  initial.weight.vec <- rep(0.0,ncol(X.scaled.mat) + 1)
  
  output <- LinearModelL1(X.scaled.mat, TrainingLabels, penalty, opt.thresh, initial.weight.vec, step.size)

  expect_true(is.null(output))
})


# set up for binary classification
test_that(
  "Tests for a successful run ",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x.scaled <- scale(zip.train[train.i,-1])
    y <- zip.train[train.i, 1]
    penalty <- 1.8
    initial.weight.vec <- rep(0.0,ncol(x.scaled))
   
    opt.thresh <- .0001
    step.size <- .8
    
    
    output <-LinearModelL1(x.scaled, y, penalty, opt.thresh, initial.weight.vec, step.size)
    
    expect_false(is.null(output))
  })


# set up for regression
test_that("Tests to see if the dimensions will be checked",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  output <- NULL
  penalty <- 2
  opt.thresh <- .00005
  step.size <- 2
  Labels <- ozone[,1] # first col is output of the data
  
  Data <- ozone[,2:ncol(ozone)]
  
  
  TrainingData <- as.matrix(Data)[1:57,]
  
  TrainingLabels <- Labels[1:57]

  
  X.scaled.mat <- scale(TrainingData)
  initial.weight.vec <- rep(0.0,ncol(X.scaled.mat) + 1)
  output <- LinearModelL1(X.scaled.mat, TrainingLabels, penalty, opt.thresh, initial.weight.vec, step.size)
  
  expect_true(is.null(output))
})
