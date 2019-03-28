library(NearestNeighbors)
library(testthat)
context("KNNLearnCV.Algorithm")

test_that("KNNLearnCV.Algorithm checks for wrong sized ford.vec", {
  data(zip.train, packages= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  fold.vec <- sample(rep(1:n.folds, l=9))
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- KNNLearnCV.Algorithm(x,y,testx,max.neighbors,fold.vec)
})


test_that(
  "KNNLearnCV.Algorithm checks for wrong contents of ford.vec",{
    data(zip.train, packages= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x <- zip.train[train.i,-1]
    y <- zip.train[train.i, 1]
    fold.vec <- sample(rep(1:9, l=x))
    testx <- zip.train[test.i, -1]
    max.neighbors <- 4
    pred.vec <- KNNLearnCV.Algorithm(x,y,testx,max.neighbors,fold.vec)
    expect_failure('The function Failed as Planned')
  })