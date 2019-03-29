library(NearestNeighbors)
library(testthat)
library(ElemStatLearn)
context("NN1toKmaxPredict")

test_that(
  "NN1toKmaxPredict checks for wrong input sizes",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x <- zip.train[train.i,-1]
    y <- zip.train[train.i, 1]
    testx <- zip.train[test.i, -1]
    max.neighbors <- 4
    pred.vec <- NN1toKmaxPredict(x,y,testx,max.neighbors)
    expect_failure('The function Failed as Planned')
  })

test_that(
  "NN1toKmaxPredict checks for wrong input sizes",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x <- zip.train[train.i,-1]
    y <- zip.train[train.i, 1]
    testx <- zip.train[test.i, -1]
    max.neighbors <- 4
    pred.vec <- NN1toKmaxPredict(x,y,testx,max.neighbors)
    expect_failure('The function Failed as Planned')
  })


test_that(
  "NN1toKmaxPredict checks for wrong input sizes",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    x <- zip.train[train.i,-1]
    y <- zip.train[train.i, 1]
    testx <- zip.train[test.i, -1]
    max.neighbors <- 4
    pred.vec <- NN1toKmaxPredict(x,y,testx,max.neighbors)
    
    expect_equal(nrow(as.matrix(pred.vec)),1)
  })
