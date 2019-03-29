
library(NearestNeighbors)
library(testthat)
context("knn")

test_that("knn computes same answer as R", {
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- knn(x,y,testx,max.neighbors)
  dist.mat <- t(x) - testx
  dist.vec <- sqrt( colSums(dist.mat * dist.mat))
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max.neighbors]
  expected.prediction <- cumsum(y[closest.indices])/(1:max.neighbors)
  expect_equal(pred.vec,expected.prediction)
})

test_that("knn checks for nulls data", {
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- knn(NULL,y,testx,max.neighbors)
  
  expect_failure('Test Failed Successfully ( as planned )')
})

test_that("knn checks for wrong sized data", {
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- knn(x[1:4,],y,testx,max.neighbors)
  expect_failure('Failed Correctly')
})
