#'K Nearest Neighbor Algorithm
#' This function uses the Commonly used algorithm, K Nearest Neighbor's to select a prediction of a given instance. 
#' This function Wraps around c++ code to call it using r. The actual implmentation is written in Cpp, and is located 
#' in the Src/knn.cpp. The R code does call the Interface.cpp implementation in the same folder. 
#'  
#' 
#' Note: This function preforms parameter checking to verify that the input parameters are correct, before calling the .cpp implementation which does more.
#'
#'@param x.mat numeric imput feature matrix [n x p]
#'@param y.vec numberic input label vector [n], 
#'either all 0/1 for binary classification or other real numbers for regression
#'@param testx.vec numberic test festure vector [p], that is used to test the different hyperparameters for k (1 to MAX.K) of KNN
#'@param max.neighbors scalar integer, max number of neighbors
#'
#'@return Returns a vector predicting the label, Y.Hat. The dimensions being [max.neighbors], for the prediction of each itteration of k. 
#'@export
#'
#'@examples
#'################### Binary Classification ######################
#'Spam<-Prep_Spam()
#'Fold.vec = Random_Folds(Spam$n_Elements,4)
#'Fold.n   = 4
#'KNNLearnCV.List = KNNLearnCV(Spam$TrainingData, Spam$TrainingLabels, 30, Fold.vec, Fold.n)
#'barplot(KNNLearnCV.List$train.loss.vec,main = "Spam: KNNLearnCV L2 Mean Error",xlab = "KNN Compared",ylab = "Error",beside = TRUE)
#'
#'######################## Regression #########################
#'

knn <- function(x.mat, y.vec, testx.vec, max.neighbors)
{
  
  result.list <- .C("knn_interface",
                    as.double(x.mat),
                    as.double(y.vec),
                    as.double(testx.vec),
                    as.integer(nrow(x.mat)),
                    as.integer(ncol(x.mat)),
                    as.integer(max.neighbors),
                    predictions=double(max.neighbors),
                    PACKAGE="NearestNeighbors")
  return(as.matrix(result.list$predictions))
}