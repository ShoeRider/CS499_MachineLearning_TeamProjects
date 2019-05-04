print(getwd())
source("R/General.R")
source("R/LinearModelL1penalties.R")
source("tests/testthat/Prep_Libraries.R")

#' Linear Model with L1 regularization using cross validation
#'
#' This algorithm splits the data into several folds and apply LinealModelL1penalites to each fold
#'
#' @param X.mat a numeric feature matrix of size n x p
#' @param y.vec a numeric labe vector of length nrow(X.mat)
#' @param fold.vec a numeric vector of lenght nrow(X.mat)
#' @param n.folds a positive integer indicate number of folds, default is 5
#' @param penalty.vec a non-negative numeric decreasing penalty vector, default is 1 to 0.1 with 0.1 decreament
#' @param step.size a positive numeric value, default is 0.1
#'
#' @return
#' @export
#'
#' @examples
#'  Spam<-Prep_Spam()
#'  folds.n = 4L
#'  Scalar.Step = 0.4
#'  max.iterations = 3000L
#'  fold.vec = as.double(sample(1:(4L),Spam$n_Elements,replace=T))
#'  Scaled.Train  = scale(Spam$TrainingData)
#'  Initial.Vector <- array(as.matrix(rep(0,NCOL(Scaled.Train)+1)),dim=c(1,NCOL(Scaled.Train)+1))
#'  y.vec <- as.numeric(Spam$TrainingLabels)
#'
#'
#' result.list = LinearModelL1CV(Scaled.Train, y.vec,fold.vec = sample(rep(1:n.folds, l = length(y.vec))),n.folds = 5L,
#' penalty.vec = seq(1, 0.1, -0.1),step.size = 0.1)
LinearModelL1CV <-
  function(X.mat,
           y.vec,
           fold.vec = sample(rep(1:n.folds, l = length(y.vec))),
           n.folds = 5L,
           penalty.vec = seq(1, 0.1, -0.1),
           step.size = 0.1) {
    if (!all(is.numeric(fold.vec),
             is.vector(fold.vec),
             length(fold.vec) == nrow(X.mat))) {
      stop("fold.vec must be a numeric vector of length nrow(X.mat)")
    }

    for (i.fold in seq(n.folds)) {
      train.vec <- (fold.vec != i.fold)

      set.list <- list(train = train.vec, validation = (!train.vec))
      for (set.name in names(set.list)) {
        index <- get(set.name, set.list)

        List <-
          LinearModelL1penalties(X.mat, y.vec, penalty.vec, step.size)
        W.mat <- List$weight.vec
        predict <- cbind(1, X.mat[index,]) %*% W.mat

        if (is.binary) {
          # Do 0-1 loss
          predict <- ifelse(predict > 0.5, 1, 0)
          loss.vec <-
            colMeans((ifelse(predict == y.vec[get(set.name, set.list)], 0, 1)))
        } else{
          # Do square loss
          loss.vec <-
            colMeans((predict - y.vec[get(set.name, set.list)]) ^ 2)
        }

        if (set.name == "train") {
          train.loss.mat[i.fold, ] <- loss.vec
        } else{
          validation.loss.mat[i.fold, ] <- loss.vec
        }
      }
    }
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    selected.penalty.index <- which.min(mean.validation.loss.vec)

    List<-
      LinearModelL1penalties(X.mat, y.vec, penalty.vec)[, selected.penalty.index]
    weight.vec = List$weight.vec

    predict <- function(testX.mat) {

      prediction.vec <- cbind(1, testX.mat) %*% weight.vec

      return(prediction.vec)
    }

    result.list <- list(
      mean.validation.loss.vec = mean.validation.loss.vec,
      mean.train.loss.vec = mean.train.loss.vec,
      penalty.vec = penalty.vec,
      selected.penalty = penalty.vec[selected.penalty.index],
      weight.vec = weight.vec,
      predict = predict
    )

    return(result.list)
  }


Test_LinearL1<-function()
{

  Spam<-Prep_Spam()

  print("Spam")

  print(dim(Spam$TrainingData))
  print(dim(Spam$TrainingLabels))
  #(Spam$TrainingData)
  #(Spam$TrainingLabels)


  n.folds = 4L
  Scalar.Step = 0.4
  max.iterations = 3000L
  fold.vec = as.double(sample(1:(4L),Spam$n_Elements,replace=T))
  Scaled.Train  = scale(Spam$TrainingData)

  if(TRUE)
  {

    Initial.Vector <- array(as.matrix(rep(0,NCOL(Scaled.Train)+1)),dim=c(1,NCOL(Scaled.Train)+1))
    #as.numeric(rep(0,NCOL(Scaled.Train)+1),dim=c(1,NCOL(Scaled.Train)+1))
    #dim(Initial.Vector) <-c(1,NCOL(Scaled.Train)+1)
    #print("dim Initial.Vector")
    #print(dim(Initial.Vector))
  }
  if(FALSE)
  {
    Initial.Vector <- as.vector(rep(0,NCOL(Scaled.Train)+1))
    #dim(Initial.Vector) <-c(1,NCOL(Scaled.Train)+1)
    #print("dim Initial.Vector")
    #print(dim(Initial.Vector))
  }




  #print(dim(Scaled.Labels))
  #print(dim(Scaled.Train))
  #print(length(Initial.Vector))
  #print(dim(Initial.Vector))
  #print("LinearModelL1")
  y.vec <- as.numeric(Spam$TrainingLabels)
  if(FALSE)
  {
    print("MEan y")
    print((y.vec))
    print((y.vec[1000:length(y.vec)]))
    print((y.vec[2000:length(y.vec)]))
    print((y.vec[3000:length(y.vec)]))
  }

  result.list = LinearModelL1CV(Scaled.Train, y.vec,
    fold.vec = sample(rep(1:n.folds, l = length(y.vec))),
    n.folds = 5L,
    penalty.vec = seq(1, 0.1, -0.1),
    step.size = 0.1)
  #w.vec = LinearModelL1(Scaled.Train,as.numeric(Spam$TrainingLabels),1,1200,Initial.Vector,5,max.iterations=1000)

  print(result.list$weight.vec)
  Loss.vec<- Find_WVector_MeanLoss(testData[,1:NCOL(testData)-1],testData[,NCOL(testData)],result.list$weight.vec,Spam$BinaryClassification)
}
#Test_LinearL1()





