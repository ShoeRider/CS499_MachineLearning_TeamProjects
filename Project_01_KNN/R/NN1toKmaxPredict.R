print(getwd())
#source("R/General.R")

#' NN1toKmaxPredict
#'
#' Wraps around c++ knn_interface code to call it using r
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n], 
#'either all 0/1 for binary classification or other real numbers for regression
#'@param TestData numberic test festure vector [p], that is used to test the different hyperparameters for k (1 to MAX.K) of KNN
#'@param max.neighbors scalar integer, max number of neighbors
#'
#'@return numeric vector of size [test.x.vec x max.neighbors] max.neighbors, predictions from 1 to max.neighbors
#'@export
#'
#'@examples
#'~~ Example 1 ~~
#'Spam<-Prep_Spam()
#'Fold.vec = Random_Folds(Spam$n_Elements,4)
#'Fold.n   = 4
#'KNNLearnCV.List = NN1toKmaxPredict(TrainingData, TrainingLabels,TestData, 30)
#' #Where KNNLearnCV.List is a list containing the elements above, and you are free to use the returned values as you wish.
#' 
#'barplot(KNNLearnCV.List$train.loss.vec,main = "Spam: KNNLearnCV.L2TestError.FoldMeans",xlab = "KNN Compared",ylab = "Error",beside = TRUE)
#'
#'~~ Example 2 ~~
#'
NN1toKmaxPredict <- function(TrainingData, TrainingLabels, TestData, max.neighbors)
{
  # n_test_observations
  #print("Here is the nrow of Test.x.vec")
  #print(dim(test.x.vec))
  result.list <- .C("NN1toKmaxPredict_interface",
                    as.integer(nrow(TrainingData)),
                    as.integer(nrow(TestData)),
                    as.integer(ncol(TrainingData)),
                    as.integer(max.neighbors),
                    as.double(TrainingData),
                    as.double(TrainingLabels),
                    as.double(TestData),
                    predictions=as.double(as.matrix(rep(0,nrow(TestData)*max.neighbors),dim=c(nrow(TestData),max.neighbors))),
                    PACKAGE="NearestNeighbors")

  dim(result.list$predictions)<- c(max.neighbors,nrow(TestData))
  return(result.list$predictions)
}
