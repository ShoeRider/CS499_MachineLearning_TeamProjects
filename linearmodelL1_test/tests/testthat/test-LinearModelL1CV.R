library(L1LinearModel)
library(testthat)
context("../../R/LinearModelL1CV.R")


# set up for regression
test_that("Tests to see if the dimensions will be checked in the CV function",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  
  Labels <- ozone[,1] # first col is output of the data
  output <- NULL
  Data <- ozone[,2:ncol(ozone)]
  
  TrainingData <- as.matrix(Data)[1:57,]
  
  TrainingLabels <- Labels[1:52]
  
  n.folds <- 5
  step.size <- 2
  X.scaled.mat <- scale(TrainingData)
  penalty.vec <- seq(1, 0.1, -0.1)
  fold.vec <- sample(rep(1:n.folds, l = length(TrainingLabels)))
  
  output <- LinearModelL1CV(X.scaled.mat, TrainingLabels, fold.vec, n.folds, penalty.vec, step.size)
  
  expect_true(is.null(output))
})


# set up for binary classification
test_that(
  "Tests for a non null / successfull output",{
    data(zip.train, package= "ElemStatLearn")
    
    i01 <- which(zip.train[,1] %in% c(0,1))
    
    train.i <- i01[1:5]
    
    test.i <- i01[6]
    
    x.scaled <- scale(zip.train[train.i,-1])
    y <- zip.train[train.i, 1]
    n.folds <- 5
    fold.vec <- sample(rep(1:n.folds, l = length(y)))
    penalty.vec <- seq(1, 0.1, -0.1)
    step.size <- .8
    
    output <- LinearModelL1CV(x.scaled, y, fold.vec, n.folds, penalty.vec, step.size)
    
    expect_false(is.null(output))
  })


test_that("Shows proper output dimensions",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  Labels <- ozone[,1] # first col is output of the data
  
  Data <- ozone[,2:ncol(ozone)]
  
  TrainingData <- as.matrix(Data)[1:57,]
  
  TrainingLabels <- Labels[1:57]
  
  n.folds <- 5
  step.size <- 2
  X.scaled.mat <- scale(TrainingData)
  penalty.vec <- seq(1, 0.1, -0.1)
  fold.vec <- sample(rep(1:n.folds, l = length(TrainingLabels)))
  
  output <- LinearModelL1CV(X.scaled.mat, TrainingLabels, fold.vec, n.folds, penalty.vec, step.size)
  
  expect_equal(length(TrainingLabels) + 1, length(output$weight.vec))
})
