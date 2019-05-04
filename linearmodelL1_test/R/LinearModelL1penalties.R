print(getwd())
source("R/General.R")
source("R/Temp.R")
source("tests/testthat/Prep_Libraries.R")

#' Linear Model with L1 regularization
#'
#' This algorithm takes the penalty vector as input and iterate through each value in the vector using LinearModelL1
#'
#'@param Normalized_TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'either all 0/1 for binary classification or other real numbers for regression
#'@param Iterations integer that determines the number of steps taken to find the optimal
#'@param StepSize.Scalar scalar integer, determines the size of each step.
#'
#' @return W.mat [n_features+1 x n_penalties], weight matrix on original scale, that can be used to get predictions via cbind(1, X.mat) %*% W.mat (the first row of W.mat should be the bias/beta/intercept)
#' @export
#'
#' @examples
#' #'  Spam<-Prep_Spam()
#'  folds.n = 4L
#'  Scalar.Step = 0.4
#'  max.iterations = 3000L
#'  fold.vec = as.double(sample(1:(4L),Spam$n_Elements,replace=T))
#'  Scaled.Train  = scale(Spam$TrainingData)
#'  Initial.Vector <- array(as.matrix(rep(0,NCOL(Scaled.Train)+1)),dim=c(1,NCOL(Scaled.Train)+1))
#'  y.vec <- as.numeric(Spam$TrainingLabels)
LinearModelL1penalties <-
  function(X.mat, y.vec, penalty.vec, step.size,opt.thresh=1200,max.iterations=1000) {
    # Check type and dimension

    is.decending <- function(vec) {
      result <- all(diff(vec) < 0)
      return(result)
    }

    if (!all(
      is.vector(penalty.vec),
      is.numeric(penalty.vec),
      penalty.vec >= 0,
      is.decending(penalty.vec)
    )) {
      stop("penalty.vec must be a non-negative decreasing numeric vector")
    }

    if (!all(is.numeric(opt.thresh),
             length(opt.thresh) == 1,
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar")
    }
    BinaryClassification <- all(ifelse(y.vec %in% c(0, 1), TRUE, FALSE))

    # Initializing
    n.train <- nrow(X.mat)
    n.features <- ncol(X.mat) # features is p here
    n.penalties <- length(penalty.vec)

    # Scale X.mat with m = 0, sd = 1
    feature.mean.vec <- colMeans(X.mat)
    feature.sd.vec <-
      sqrt(rowSums((t(X.mat) - feature.mean.vec) ^ 2) / n.train)

    # column with zero variance will become zero at the end
    feature.sd.vec[feature.sd.vec == 0] <- 1

    feature.sd.mat <- diag(1 / feature.sd.vec)

    X.scaled.mat <-
      t((t(X.mat) - feature.mean.vec) / feature.sd.vec)


    #Initial.Vector <- array(as.matrix(rep(0,NCOL(X.scaled.mat)+1)),dim=c(1,NCOL(X.scaled.mat)+1))


    W.Matrix <- array(as.matrix(rep(0,NCOL(X.scaled.mat)+1)),dim=c(1,NCOL(X.scaled.mat)+1))
    #print(dim(W.Matrix))
    # W.temp.mat <- W.mat
    L1Error.vector<- Find_WVector_MeanLoss(X.scaled.mat, y.vec,t(W.Matrix),BinaryClassification)
    W.vec<-0
    for (i.penalty in c(1:n.penalties)) {
      #print("iteration")
      #print(i.penalty)
      #print(length(W.vec))
      #print(dim(W.vec))
      List <-
        (LinearModelL1(X.scaled.mat,
                      y.vec,
                      penalty.vec[i.penalty],
                      opt.thresh,
                      t(W.Matrix[i.penalty,]),
                      step.size,max.iterations=50))
      W.vec <- List$weight.vec
      #print("returned from LML1")
      #LinearModelL1(Scaled.Train,y.vec,0.5,1200,Initial.Vector,.5,max.iterations=50)
      #print(length(W.vec))
      #print(dim(W.vec))
      #print(dim(W.Matrix))

      L1_norm.val <- Find_WVector_MeanLoss(X.scaled.mat, y.vec,(W.vec),BinaryClassification)
      L1Error.vector<-rbind(L1Error.vector,L1_norm.val)

      W.Matrix <- rbind(W.Matrix,t(W.vec))
    }

  print(dim(L1Error.vector))
  print(length(L1Error.vector))
    SmallestLoss = .Machine$integer.max
    selected.steps = 0
    for(Index in 1:length(L1Error.vector))
    {
      print(L1Error.vector[Index])
      if(L1Error.vector[Index] < SmallestLoss)
      {
        SmallestLoss    = L1Error.vector[Index]
        selected.steps  = Index
        Best.W = W.Matrix[Index,]
      }
    }

    #W.mat (n_features+1 x n_penalties), weight matrix on original scale, that can be used to get predictions via
    #cbind(1, X.mat) %*% W.mat (the first row of W.mat should be the bias/beta/intercept)
    #return(W.Matrix)
    result.list <- list(
      L1Error.vector = L1Error.vector,
      selected.steps = selected.steps,
      weight.mat = W.Matrix,
      weight.vec = as.vector(Best.W)
    )
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
#(X.mat, y.vec, penalty.vec, step.size)
  List = LinearModelL1penalties(Scaled.Train, y.vec,penalty.vec = seq(1, 0.1, -0.1),step.size = 0.1,opt.thresh=300,max.iterations=1000)
  #w.vec = LinearModelL1(Scaled.Train,as.numeric(Spam$TrainingLabels),1,1200,Initial.Vector,5,max.iterations=1000)
W.Matrix  = List$weight.mat

print("Returned")
  print(dim(W.Matrix))
  print(NROW(W.Matrix))
  print(dim(as.matrix(W.Matrix[NROW(W.Matrix),])))



  print(dim(as.matrix(List$weight.vec)))
  Loss<- Find_WVector_MeanLoss(Scaled.Train,y.vec,W.Matrix[NROW(W.Matrix),],Spam$BinaryClassification)
  print(dim(List$weight.vec))

}
#Test_LinearL1()
