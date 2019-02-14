#'K Nearest Neighbor Algorithm
#'
#' Wraps around c++ code to call it using r
#'
#'@param x.mat numeric imput feature matrix [n x p]
#'@param y.vec numberic input label vector [n], 
#'either all 0/1 for binary classification or other real numbers for regression
#'@param testx.vec numberic test festure vector [p]
#'@param max.neighbors scalar integer, max number of neighbors
#'
#'@return numeric vector of size max.neighbors, predictions from 1 to max.neighbors
#'@export
#'
#'@examples
#'data(zip.train, packages= "ElemStatLearn")
#'i01 <- which(zip.train[,1] %in% c(0,1))
#'train.i <- i01[1:5]
#'test.i <- i01[6]
#'x <- zip.train[train.i,-1]
#'y <- zip.train[train.i, 1]
#'testx <- zip.train[test.i, -1]
#'knn(x,y,testx,3)
#'zip.train[test.i,1]
#'
knn <- function(x.mat, y.vec, testx.vec, max.neighbors)
{
  
  result.list <- .C("knn_interface", as.double(x.mat), as.double(y.vec),
                    as.double(testx.vec),as.integer(nrow(x.mat)),
                    as.integer(ncol(x.mat)),as.integer(max.neighbors),
                    predictions=double(max.neighbor),PACKAGE="nearestNeighborsAlg")
}

