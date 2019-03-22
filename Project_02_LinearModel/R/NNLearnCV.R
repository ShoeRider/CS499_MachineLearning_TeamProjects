#library(NearestNeighbors)
#library(testthat)
#context("knn")


#print(getwd())
#source("R/General.R")
#source("R/NN1toKmaxPredict.R")
#R CMD build



#' KNNLearnCV.Algorithm
#'
#' Simple funtion that calls NN1toKmaxPredict.R function, and finds the most opatimal K value for KNN.
#' Returns a list of the produced Error Mean, a function wich uses the found hyperparameter K, with Lowest Error.
#'
#'
#'@param TrainingData numeric imput feature matrix [n x p],
#'@param TrainingLabels numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param TestData numberic test festure vector [p], that is used to test the different hyperparameters for k (1 to MAX.K) of KNN
#'@param max.neighbors scalar integer, max number of neighbors
#'@param fold.vec vector indicating the fold each element is in [n]
#'@param fold.n the number of folds that are to be preformed
#'
#'@return Returns a list with the following elements
#'      X.mat              = TrainingData numeric imput feature matrix [n x p],
#'      y.vec              = TrainingLabels numberic input label vector [n],
#'      train.loss.mat     =
#'      train.loss.vec     = Mean Error for each 1toKmax solution,
#'      selected.neighbors = The integer value of the otimal K for KNN,
#'      Predict            = Predict(X.), Function to preform the optimal K nearest neighbors selected,
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
#'Fold.vec = Random_Folds($n_Elements,4)
#'Fold.n   = 4
#'KNNLearnCV.List = NN1toKmaxPredict(TrainingData, TrainingLabels,TestData, 30)
#' #Where KNNLearnCV.List is a list containing the elements above, and you are free to use the returned values as you wish.
#'
#'barplot(KNNLearnCV.List$train.loss.vec,main = "Spam: KNNLearnCV.L2TestError.FoldMeans",xlab = "KNN Compared",ylab = "Error",beside = TRUE)
KNNLearnCV.Algorithm<-function(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  Data     = cbind(X.mat ,Y.vec)
  NonZero  = 0
  BinaryClassification =  all(Y.vec == 1 & Y.vec == 0)

  print(BinaryClassification)

  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)

  #print(Rows)




  L2TestError.Matrix = 0
  L2TrainError.Matrix = 0
  #print(folds.vec)
  #loop over (n.folds)folds validation
  for(i in 1:n.folds){
    #Segement your data by fold using the which() function
    testIndexes <- which(fold.vec==i,arr.ind=TRUE)
    #Create Fold Matrixes
    test.FoldData <- as.matrix(Data[testIndexes, ])
    train.FoldData <- as.matrix(Data[-testIndexes, ])

    #Separate InputData and InputLabels
    test.Data    <- as.matrix(test.FoldData[,DataColsStart:DataColsEnd])
    test.Labels  <- as.matrix(test.FoldData[,LabelCol])

    train.Data   <- as.matrix(train.FoldData[,DataColsStart:DataColsEnd])
    train.Labels <- as.matrix(train.FoldData[,LabelCol])

    Prediction.1ToMaxPrediction <- NN1toKmaxPredict(train.Data, train.Labels, test.Data, max.neighbors)
    L2TestError.1ToMax.Vector = Find.L2ErrorMean(Prediction.1ToMaxPrediction,test.Labels,BinaryClassification)

    Prediction.1ToMaxPrediction <- NN1toKmaxPredict(train.Data, train.Labels, train.Data, max.neighbors)
    L2TrainError.1ToMax.Vector = Find.L2ErrorMean(Prediction.1ToMaxPrediction,train.Labels,BinaryClassification)
    #print("Something cool")

    L2TestError.Matrix = rbind(L2TestError.Matrix,t(L2TestError.1ToMax.Vector))
    L2TrainError.Matrix = rbind(L2TrainError.Matrix,t(L2TrainError.1ToMax.Vector))

  }
  TrainMeanError.mat <-as.matrix(L2TrainError.Matrix[2:nrow(L2TrainError.Matrix),])
  dim(TrainMeanError.mat) = c(nrow(L2TestError.Matrix)-1,max.neighbors)
  TrainMeanError.Means = as.matrix(colMeans(TrainMeanError.mat))
  dim(TrainMeanError.Means)<- c(1,max.neighbors)


  TestMeanError.mat<- as.matrix(L2TestError.Matrix[2:nrow(L2TestError.Matrix),])
  dim(TestMeanError.mat) = c(nrow(L2TestError.Matrix)-1,max.neighbors)
  TestMeanError.Means = as.matrix(colMeans(TestMeanError.mat))
  dim(TestMeanError.Means)<- c(1,max.neighbors)

  #select Smallest Loss

  SmallestLoss = .Machine$integer.max
  selected.KNN = 0
  for(Index in 1:max.neighbors)
  {
    if(TestMeanError.Means[Index] < SmallestLoss)
    {
      SmallestLoss    = TestMeanError.Means[Index]
      selected.KNN    = Index
    }
  }
  #print("Selected KNN")
  #print(selected.KNN)




  Predict = Predict<-function(X.Features)
  {
    # type demesion check
    if (!all(is.numeric(testX.mat),
             is.matrix(testX.mat),
             ncol(testX.mat) == ncol(X.mat))) {
      stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
    }

    prediction.result <-NN1toKmaxPredict(X.mat, y.vec, testX.mat, as.integer(selected.neighbors))
    prediction.vec <- prediction.result[, selected.neighbors]
    if (label.is.binary)
    {
      prediction.vec <- ifelse(prediction.vec > 0.5, 1, 0)
    }
    return(prediction.vec)
  }




  #return a list with the following named elements:
  #  X.mat, y.vec: training data.
  #  train.loss.mat, validation.loss.mat (matrices of loss values for each fold and number of neighbors).
  #  train.loss.vec, validation.loss.vec (vectors with max.neighbors elements: mean loss over all folds).
  #  selected.neighbors (number of neighbors selected by minimizing the mean validation loss).
  #  predict(testX.mat), a function that takes a matrix of inputs/features and returns a vector of predictions. It should check the type/dimension of testX.mat and stop() with an informative error message if there are any issues.
  ReturnList <-
    list(
      X.mat              = X.mat,
      y.vec              = Y.vec,
      TestMeanError.mat    = TestMeanError.mat,
      TestMeanError.Means  = TestMeanError.Means,
      TrainMeanError.mat   = TrainMeanError.mat,
      TrainMeanError.Means = TrainMeanError.Means,
      selected.KNN       = selected.KNN,
      Predict            = Predict
    )
}





