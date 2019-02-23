#' Compute mean using pure C
#'
#' @param x.vec vector with at least 1 element, which is coerced to integer
#'
#' @return mean, numeric/double scalar
#' @export
#'
#' @examples
#' my_mean_R(c(1,2))
my_mean_Test <- function(){
  res.list <- .C(
    "LM_FindPrediction_interface",
    as.double(c(1,2)),
    as.double(c(1,2)),
    as.double(c(1,2)),
    as.integer(1),
    as.integer(1),
    as.double(c(1,2)),
    as.double(1),
    as.integer(1),
    PACKAGE="LinearModel")
}

LMLogisticRegression_FindPrediction <- function(TrainingData, TrainingLabels,TestData,Scalars,Bias){
  res.list <- .C(
    "LMLogisticRegression_FindPrediction_interface",
    as.double(TrainingData),
    as.double(TrainingLabels),
    as.double(TestData),
    as.integer(NCOL(TrainingData)),
    as.integer(NROW(TrainingData)),
    as.double(Scalars),
    as.double(Bias),
    test_predict_ptr = as.double(NROW(TrainingData)),
    PACKAGE="LinearModel")
}



LMLogisticLossEarlyStoppingCV<-function(X.mat, y.vec, fold.vec, max.iterations.)
{

}


LMLogisticLossIterations<-function(TrainingData, # X.mat (feature matrix, n_train x n_features),
                                   TrainingLabels, # y.vec (label vector, n_train x 1),
                                   Iterations, # max.iterations (int scalar > 1),
                                   StepSize.Scalar # int step.siz
                                   )
{
  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  scaled.input.data = NormalizeMatrix(TrainingData)
  # keep track the mean/sd of each column, so you can return W.mat on the original scale
  col.sd.vec = as.numeric(-1,ncol(scaled.input.data))
  for( col in 1:ncol(scaled.input.data))
  {
    # save a vector of the sd of each column
    col.sd.vec[col] <- sd(scaled.input.data[,col])
  }
  # print(col.sd.vec)


  # Value<-LM_FindPrediction(TrainingData, TrainingLabels,TestData,Scalars,Bias)
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  if( BinaryClassification )
  {
    # BinaryClassification...
  }
  else
  {
    # else Regression ...
  }
  # function should optimize the mean loss (not the total loss).

  # returns matrix of weight vectors, one for each iteration, n_features x max.iterations.
}

