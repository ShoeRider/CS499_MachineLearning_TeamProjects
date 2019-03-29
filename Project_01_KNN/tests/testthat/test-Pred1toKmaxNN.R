library(NearestNeighbors)
library(testthat)
library(ElemStatLearn)
context("NN1toKmaxPredict")

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

test_that(
  "NN1toKmaxPredict checks for wrong input sizes",{
    data(zip.train, package= "ElemStatLearn")
    i01 <- which(zip.train[,1] %in% c(0,1))
    train.i <- i01[1:5]
    test.i <- i01[6]
    y <- zip.train[train.i-1, 1]
    testx <- zip.train[test.i, -1]
    max.neighbors <- 4
    pred.vec <- NN1toKmaxPredict(cbind(1,2),y,testx,max.neighbors)
    expect_true(is.null(pred.vec))
  })


test_that(
  "NN1toKmaxPredict checks for correct output sizes",{
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
    expect_equal(length(pred.vec),nrow(testx))
  })
