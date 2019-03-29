library(NearestNeighbors)
library(testthat)
context("KNNLearnCV.Algorithm")

test_that("KNNLearnCV.Algorithm checks for wrong sized ford.vec", {
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  fold.vec <- sample(rep(1:n.folds, l=nrow(x)))
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- KNNLearnCV.Algorithm(x,y,testx,max.neighbors,fold.vec)
})

test_that("KNNLearnCV.Algorithm runs non binary correcly", {
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(4,6))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  n.folds <- 4
  fold.vec <- sample(rep(1:n.folds, l=nrow(x)))
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- KNNLearnCV.Algorithm(x,y,testx,max.neighbors,fold.vec)
})

test_that(
  "KNNLearnCV.Algorithm checks for wrong contents of ford.vec",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x <- zip.train[train.i,-1]
    y <- zip.train[train.i, 1]
    fold.vec <- sample(rep(1:9, l=5))
    testx <- zip.train[test.i, -1]
    max.neighbors <- 4
    pred.vec <- KNNLearnCV.Algorithm(x,y,testx,max.neighbors,fold.vec)
    expect_failure('The function Failed as Planned')
  })

test_that("KNNLearnCV.Algorithm checks for wrong sized prediction vector", {
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  fold.vec <- sample(rep(1:n.folds, l=nrow(x)))
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- KNNLearnCV.Algorithm(x,y,testx,max.neighbors,fold.vec)
  expect_equal(nrow(as.matrix(pred.vec)), 1)
})
