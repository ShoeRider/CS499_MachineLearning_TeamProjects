library(L1LinearModel)
library(testthat)
context("../../R/LinearModelL1penalties.R")


# set up for regression
test_that("Tests to see if the dimensions will be checked in the penalties function",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  
  output <- NULL
  Labels <- ozone[,1] # first col is output of the data
  
  Data <- ozone[,2:ncol(ozone)]
  
  TrainingData <- as.matrix(Data)[1:57,]
  
  TrainingLabels <- Labels[1:52]
  
  step.size <- 2
  
  X.scaled.mat <- scale(TrainingData)
  penalty.vec <- seq(1, 0.1, -0.1)
  
  
  output <- LinearModelL1penalties(X.scaled.mat, TrainingLabels, penalty.vec, step.size)
  
  expect_true(is.null(output))
})


# set up for binary classification
test_that(
  "Tests for a successful run ",{
    data(zip.train, package= "ElemStatLearn")
    
    i01 <- which(zip.train[,1] %in% c(0,1))
    
    train.i <- i01[1:5]
    
    test.i <- i01[6]
    
    x.scaled.mat <- scale(zip.train[train.i,-1])
    y <- zip.train[train.i, 1]
    
    penalty.vec <- seq(1, 0.1, -0.1)
    show(penalty.vec)
    step.size <- .8
    
    output <- LinearModelL1penalties(x.scaled.mat, y, penalty.vec, step.size)
    
    expect_equal((length(y) +1) == (nrow(output)))
    expect_equal(ncol(output) == length(penalty.vec))
  })