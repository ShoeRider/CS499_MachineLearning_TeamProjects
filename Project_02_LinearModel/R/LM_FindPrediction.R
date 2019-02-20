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


# Value<-LM_FindPrediction(TrainingData, TrainingLabels,TestData,Scalars,Bias)
# if BinaryClassification...
# else Regression ...
#

