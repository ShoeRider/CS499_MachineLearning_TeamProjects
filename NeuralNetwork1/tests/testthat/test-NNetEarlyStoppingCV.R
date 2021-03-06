library(NeuralNetwork1)
library(testthat)
context("../R/Neuralnetwork")

test_that("NNetEarlyStoppingCV test size of input: clean run",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  iterations <- as.integer(20)
  n.hidden.units <- 5
  step.size <- .1
  n.folds <- 4
  Labels <- ozone[,1] # first col is output of the data
  #show(Labels)
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels))
  fold.vec <- sample(rep(1:n.folds), length(TrainingLabels),replace = TRUE)

  output <- NNetEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec, iterations,step.size,n.hidden.units,n.folds)
  #show(output)
  succeed()
})

test_that("NNetEarlyStoppingCV test iteration type",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  iterations <- as.numeric(20)
  n.hidden.units <- 5
  step.size <- .1
  n.folds <- 4
  Labels <- ozone[,1] # first col is output of the data
  #show(Labels)
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels))
  fold.vec <- sample(rep(1:n.folds), length(TrainingLabels),replace = TRUE)

  # iteration is not an integer
  expect_error(NNetEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec, iterations,step.size,n.hidden.units,n.folds))
})

test_that("NNetEarlyStoppingCV test iteration type",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  iterations <- as.numeric(20)
  n.hidden.units <- 5
  step.size <- .1
  n.folds <- 4
  Labels <- ozone[,1] # first col is output of the data
  # show(Labels)
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]

  fold.vec <- sample(rep(1:n.folds), length(TrainingData),replace = TRUE)

  # iteration is not an integer
  expect_error(NNetEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec, iterations,step.size,n.hidden.units,n.folds))
})


test_that("NNetEarlyStoppingCV test fold vector size validation",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  iterations <- as.integer(20)
  n.hidden.units <- 5
  step.size <- .1
  n.folds <- 4
  Labels <- ozone[,1] # first col is output of the data
  #show(Labels)
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels))
  fold.vec <- sample(rep(1:n.folds), length(TrainingData - 2),replace = TRUE)

  expect_error(NNetEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec, iterations,step.size,n.hidden.units,n.folds))
})

test_that("NNetEarlyStoppingCV test output vector size",{
  data(ozone, package = "ElemStatLearn")
  # show(ozone)
  iterations <- as.integer(20)
  n.hidden.units <- 5
  step.size <- .1
  n.folds <- 4
  Labels <- ozone[,1] # first col is output of the data
  #show(Labels)
  Data <- ozone[,2:ncol(ozone)]


  fold.vec <- sample(rep(1:n.folds), length(Data),replace = TRUE)

  Data <- as.matrix(as.data.frame(lapply(Data, as.numeric)))
  is.numeric(Data)

  output <- NNetEarlyStoppingCV(Data, Labels, fold.vec, iterations,step.size,n.hidden.units,n.folds)

  expect_equal(as.integer(length(output$mean.train.loss.vec)),as.integer(length(output$mean.validation.loss.vec)))
})
