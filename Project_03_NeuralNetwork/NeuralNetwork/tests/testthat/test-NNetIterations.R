library(NeuralNetwork)
library(testthat)
context("../R/NNetIterations")

test_that("NNetIterations test size of input: clean run",{
  data(ozone, package = "ElemStatLearn")

  iterations <- 20
  n.hidden.units <- 5
  step.size <- .1
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels))

  output <- NNetIterations(TrainingData,TrainingLabels,iterations,step.size,n.hidden.units,is.train.vec)
  show(output)
  succeed()
})

test_that("NNetIterations catches wrong sized is.train vec correctly",{
  data(ozone, package = "ElemStatLearn")
  iterations <- 20
  n.hidden.units <- 5
  step.size <- .1
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels) - 1)
  expect_error(NNetIterations(TrainingData,
                                TrainingLabels,
                                iterations,
                                step.size,
                                n.hidden.units,
                                is.train.vec))
})

test_that("NNetIterations catches wrong Y.vec size",{
  data(ozone, package = "ElemStatLearn")
  iterations <- 20
  n.hidden.units <- 5
  step.size <- .1
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels))
  expect_error(NNetIterations(TrainingData,
                              TrainingLabels[2:-1],
                              iterations,
                              step.size,
                              n.hidden.units,
                              is.train.vec))
})

test_that("NNetIterations catches wrong is.train.vec contents",{
  data(ozone, package = "ElemStatLearn")
  iterations <- 20
  n.hidden.units <- 5
  step.size <- .1
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  is.train.vec <- sample(c(3,1,6), replace = TRUE, size = length(TrainingLabels))

  expect_error(NNetIterations(TrainingData,
                              TrainingLabels,
                              iterations,
                              step.size,
                              n.hidden.units,
                              is.train.vec))
})
