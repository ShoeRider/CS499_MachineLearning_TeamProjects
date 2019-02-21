#' Compute mean using pure C
#'
#' @param x.vec vector with at least 1 element, which is coerced to integer
#'
#' @return mean, numeric/double scalar
#' @export
#'
#' @examples
#'
#'

NormalizeMatrix<-function(TrainingData)
{
  TrainingData**2
}


SS_Fit<-function(TrainingData,Scalars,Bias)
{
  #print(colMeans(TrainingData))
  SS_Mean = (TrainingData - colMeans(TrainingData))**2
}
SS_Mean<-function(TrainingData)
{
  #print(colMeans(TrainingData))
  SS_Mean = (TrainingData - colMeans(TrainingData))**2
}
Var_Mean<-function(TrainingData)
{
  Var_Mean = SS_Mean(TrainingData)/NROW(TrainingData)
}
Var_Mean<-function(TrainingData)
{
  Var_Mean = SS_Mean(TrainingData)/NROW(TrainingData)
}
R_Squared<-function(TrainingData)
{

}
LMSquare_Prediction<-function(TrainingData,TrainingLabels,W.mat,Bias=0)
{

}
#X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
LMSquareLossIterations<-function(TrainingData, TrainingLabels,Iterations,StepSize.Scalar)
{
  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  # the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat
  # during gradient descent, it will not converge – numerical instability).
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)


  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)

  W.mat = numeric(LabelCol)
  #Training.L1Error.matrix = 0
  Training.L2Error.matrix = 0

  #Testing.L1Error.matrix = 0
  #Testing.L2Error.matrix = 0
  for(Iteration in 1:Iterations)
  {
    result<-LMSquare_Prediction(TrainingData,TrainingLabels,W.mat,Bias=0)
    #Least Squares
    #sum of squared residuals
    SSR = (result - TrainingLabels)**2
    #Slope of SSR
    Slope = 2*(colMeans(W.mat) - TrainingLabels)
  }





  # optimize the mean loss (not the total loss).

}

LMSquareLossEarlyStoppingCV<-function(X.mat, y.vec, fold.vec, max.iterations)
{
  Data = cbind(TrainingData ,TrainingLabels)
  #Randomly shuffle the data
  yourData<-yourData[sample(nrow(yourData)),]

  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    trainData <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
}
#penalty  (non-negative numeric scalar)
#opt.thresh (positive numeric scalar)
LMSquareLossL2<-function(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec.)
{

}
#penalty.vec (vector of decreasing penalty values)
LMSquareLossL2penalties<-function(X.mat, y.vec,penalty.vec)
{

}


LMSquareLossL2CV<-function(X.mat, y.vec, fold.vec, penalty.vec)
{

}






