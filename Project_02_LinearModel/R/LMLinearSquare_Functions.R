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
#'
#'
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}

NormalizeMatrix<-function(Matrix)
{
  # vector of mean column values
  mean <- mean(Matrix)
  # sum all of all columns for each row
  sum = 0
  for( row in 1:nrow(Matrix))
  {
    sum = sum + sum((Matrix[row,] - mean)^2)
  }

  # get sd from the calculated sum and number of observations
  sd = sqrt(sum / length(Matrix))
  # return the new matrix
  print("sd")
  print(sd)
  return((Matrix - mean)/sd)
}

DeNormalizeMatrix<-function(Matrix,OriginalMean,OriginalSum)
{
  0
}
NormalizeVector<-function(Vector)
{
  return<- Vector /sum(Vector)
}


LMSquare_Gradient<-function(TrainingData,TrainingLabels,W.Vector)
{
  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
  Y.hat = data.matrix(TrainingData) %*% data.matrix(W.Vector)


  SquaredNorm = (Y.hat - TrainingLabels)

  # with a scalar of 2 , but its not needed becasue This vector will be normalized
  Gradient = (t(TrainingData) %*% SquaredNorm)
}
LMSquare_L2Error<-function(TrainingData,TrainingLabels,W.Vector,BinaryClassification)
{
  Y.hat = data.matrix(TrainingData) %*% data.matrix(W.mat[,Iteration])
  if(BinaryClassification)
  {
    Error_Vector = ifelse(Y.hat>0.5,1,0) != as.integer(TrainingLabels)
    #Error_Vector = ((Y.hat) - as.integer( TrainingLabels))**2
  }else{
    Error_Vector = ((Y.hat) - as.integer( TrainingLabels))**2
  }
  #print(Error_Vector)

  L2Error_Vector = (Error_Vector)
  #print(L2Error_Vector)
  ErrorSum = sum(L2Error_Vector)
  print("Y.hat")
  print(Y.hat[1:2,])
  print("TrainingLabels")
  print(TrainingLabels[1:2])
  L2Error.matrix <- rbind(L2Error.matrix, ErrorSum)
}

#Find_Wmatrix_L2Error
#Takes: (W.Matrix,TestingData.normalized,TestingLables,BinaryClassification)
#Note:
#Calculates the ((Y.hat) - (Y.Truth))**2
#With exception to BinaryClassification models
# were Y.Hat is determined to be 0,1 and compaired to Y.Truth
# Error Is either 0 or 1.
#returns: vector of fitness of every W.Matrix Row
Find_Wmatrix_MeanL2Error<-function(TestingData,TestingLables,W.Matrix,BinaryClassification)
{
  L2Error.matrix = 0
  Y.hat = data.matrix(TestingData) %*% data.matrix(W.Matrix)
  if(BinaryClassification)
  {
    Error_Vector = ifelse(Y.hat>0.5,1,0) != as.integer(TestingLables)
    #Error_Vector = ((Y.hat) - as.integer( TestingLables))**2
  }else{
    Error_Vector = ((Y.hat) - as.integer(TestingLables))**2
  }
  L2Error.Matrix = (as.matrix(colMeans(Error_Vector)))
}


#X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
LMSquareLossIterations<-function(TrainingData, TrainingLabels,Iterations,StepSize.Scalar)
{
  TrainingData <- data.matrix(TrainingData)
  TrainingData.mean <- mean(TrainingData)
  TrainingData.Sum = 0

  for( row in 1:nrow(TrainingData))
  {
    TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  }

  # get sd from the calculated sum and number of observations
  TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))

  Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #TrainingLabels <- data.matrix(TrainingLabels)

  print("Starting LMSquareLossIterations")
  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  # the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat
  # during gradient descent, it will not converge – numerical instability).
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #initial Matrix for iteration 1
  W.Matrix = array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))
  Error = 0


  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {

    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,])
    NormalizedGradient <- data.matrix(NormalizeVector(Gradient))*StepSize.Scalar
    W.Matrix <- rbind(W.Matrix, W.Matrix[Iteration,]+t(NormalizedGradient))

  }


  #Norm.Error <-Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification)
  #print(Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification))


  #DeNormalized.TrainingData<-((Normalized_TrainingData)*TrainingData.sd)+TrainingData.mean
  DeNormalizedWeightMatrix = (t(W.Matrix)*mean(W.Matrix))-t(W.Matrix/TrainingData.sd)



  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(TrainingData,TrainingLabels,DeNormalizedWeightMatrix,BinaryClassification)
  #Norm.Error   <-Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification)

  return(DeNormalizedWeightMatrix)
}

LMSquareLossIterations.DeNormalizationError<-function(TrainingData, TrainingLabels,Iterations,StepSize.Scalar)
{
  TrainingData <- data.matrix(TrainingData)
  TrainingData.mean <- mean(TrainingData)
  TrainingData.Sum = 0

  for( row in 1:nrow(TrainingData))
  {
    TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  }

  # get sd from the calculated sum and number of observations
  TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))

  Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #TrainingLabels <- data.matrix(TrainingLabels)

  print("Starting LMSquareLossIterations")
  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  # the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat
  # during gradient descent, it will not converge – numerical instability).
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #initial Matrix for iteration 1
  W.Matrix = array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))
  Error = 0


  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {

    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,])
    NormalizedGradient <- data.matrix(NormalizeVector(Gradient))*StepSize.Scalar
    W.Matrix <- rbind(W.Matrix, W.Matrix[Iteration,]+t(NormalizedGradient))

  }

  Norm.Error <-Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification)
  #print(Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification))
  barplot(
    Norm.Error,
    main = "Norm.Error: spam",
    xlab = "mean loss value",
    beside = TRUE
  )
  #print(Error)

  #clearly not how to find the W.DeNormalizedMatrix
  #print(W.Matrix)
  W.Matrix.mean <- mean(W.Matrix)

  W.Matrix.Sum = 0

  for( col in 1:ncol(W.Matrix))
  {

    W.Matrix.Sum = W.Matrix.Sum + sum((W.Matrix[,col] - W.Matrix.mean)^2)
  }

  # get sd from the calculated sum and number of observations
  W.Matrix.sd = sqrt(W.Matrix.Sum / length(W.Matrix))
  print(W.Matrix.sd)
  print(TrainingData.sd)


  DeNormalized.TrainingDatam<-((Normalized_TrainingData)*TrainingData.sd)+TrainingData.mean

  DeNormalizedWeightMatrix = (t(W.Matrix)*mean(W.Matrix))-t(W.Matrix/TrainingData.sd)

  # print(sum(abs(DeNormalized.TrainingDatam-TrainingData)))

  DeNorm.Error <-Find_Wmatrix_MeanL2Error(TrainingData,TrainingLabels,DeNormalizedWeightMatrix,BinaryClassification)
  #Error<-Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification)


  Error = abs(Norm.Error - DeNorm.Error)
  #print(Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,W.DeNormalizedMatrix,BinaryClassification))
  barplot(
    Error,
    main = "DeNorm.Error: spam",
    xlab = "mean loss value",
    beside = TRUE
  )
  #print(Error)
  return(DeNormalizedWeightMatrix)
}


#LMSquareLossEarlyStoppingCV
#Takes: (TrainingData, TrainingLabels, fold.vec,folds.n=4,max.iterations)
#
LMSquareLossEarlyStoppingCV<-function(TrainingData, TrainingLabels, fold.vec,folds.n=4,max.iterations)
{
  step.size = 2
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  Data = cbind(TrainingData ,TrainingLabels)
  #Randomly shuffle the data
  Data<-Data[sample(nrow(Data)),]

  #TODO If invalid fold.vec .. make one for ourselves

  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)

  #initialize Error Matrix:
  L2Error.Matrix = 0
  #Perform folds.n fold cross validation
  for(i in 1:folds.n){
    #Segement your data by fold using the which() function
    testIndexes <- which(fold.vec == i,arr.ind=TRUE)
    testData <- Data[testIndexes, ]
    trainData <- Data[-testIndexes, ]

    test.Data    <- testData[,DataColsStart:DataColsEnd]
    test.Labels  <- testData[,LabelCol]

    train.Data   <- trainData[,DataColsStart:DataColsEnd]
    train.Labels <- trainData[,LabelCol]

    if(typeof(test.Data) =="double")
    {
      test.Data   <- test.Data
    }else{
      test.Data   <- do.call(cbind, lapply(test.Data, as.numeric))
    }

    if(typeof(test.Labels) =="double")
    {
      test.Labels   <- test.Labels
    }else{
      test.Labels    <- do.call(cbind, lapply(test.Labels, as.numeric))
    }

    #for each train/validation split, use LM___LossIterations to compute a sequence of models

    W.Matrix         =  LMSquareLossIterations(train.Data,train.Labels,max.iterations,step.size)

    #on the train data, then compute the validation loss of each model.
    Error            =  Find_Wmatrix_MeanL2Error(test.Data,test.Labels,t(W.Matrix),BinaryClassification)
    L2Error.Matrix   <- rbind(L2Error.Matrix,t(Error))
  }

  #compute mean.validation.loss.vec, which is a vector (with max.iterations elements)
  # of mean validation loss over all K folds.
  mean.validation.loss = colMeans(L2Error.Matrix)


  #minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
  SmallestLoss = .Machine$integer.max
  selected.steps = 0
  for(Index in 1:max.iterations)
  {
    if(mean.validation.loss[Index] < SmallestLoss)
    {
      SmallestLoss = mean.validation.loss[Index]
      selected.steps        = Index
    }
  }

  #finally use LMLinearSquareLossIterations(max.iterations=selected.steps) on the whole training data set.
  MainWeight.Vector = LMSquareLossIterations(TrainingData, TrainingLabels,selected.steps,step.size)

  #return a list with:
  #mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
  #selected.steps
  #weight.vec, the weight vector found by using gradient descent with selected.steps on the whole training data set.
  #predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).

  #not sure how to return list
  ReturnList = 0
  #[mean.validation.loss,selected.steps,MainWeight.Vector,function(Feature.Matrix){return(Feature.Matrix %*% MainWeight.Vector) }]
}


#LMSquareLossL2
#--------------------------------------------------------------------------------------------------------------
#Finds the optimal weight vector that minimizes the following cost function:
# Sum   (L[w^T x_i, y_i] + penalty * ||w|| )
#i=1^n

#penalty  (non-negative numeric scalar)

#Gradient cliping ?
#opt.thresh (positive numeric scalar)
#opt.thresh should be a threshold on the L1-norm (sum of absolute values) of the gradient.
#I will test your code to make sure that the L1-norm of the gradient of your solution is less than opt.thresh.

LMSquareLossL2<-function(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)
{
  print("Starting LMSquareLossL2")
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #initial Matrix for iteration 1
  W.mat = array(initial.weight.vec,dim=c(1,NCOL(Normalized_TrainingData)))

  #Itterate till Cur.thresh < opt.thresh of times
  Cur.thresh = .Machine$integer.max
  validation.loss = 0
  Iteration = 0
  print(W.mat[1,])
  while (!(Cur.thresh < opt.thresh) && !(Iteration == 2))
  {
    Iteration = 1+Iteration

    #ind the optimal weight vector that minimizes the following cost function:
    # Sigma   L[w^T x_i, y_i] + penalty * ||w||
    #i=1^n
    #where L is either the logistic or square loss.
    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.mat[Iteration,])
    RegularizedGradient <- Gradient + penalty*(colSums(W.mat**2))**(1/2)
    #Gradient clip

    print(Gradient)
    #apply(RegularizedGradient, function(y) min(max(y,(-opt.thresh)),(opt.thresh)))
    W.mat <- rbind(W.mat, W.mat[Iteration,]+t(RegularizedGradient))
  }

  #Output: optimal weight vector for the given penalty parameter.
  W.mat
}


#penalty.vec (vector of decreasing penalty values)
LMSquareLossL2penalties<-function(X.mat, y.vec,penalty.vec)
{
  #this function should begin by scaling X.mat to obtain X.scaled.mat with each column mean=0 and sd=1
  X.mat.mean = sum(X.mat)/NROW(X.mat)
  Normalized_TrainingData<-NormalizeMatrix(X.mat)
  opt.thresh = 1

  W.mat = 0
  for(penalty in penalty.vec)
  {
    W.vec <- LMSquareLossL2(Normalized_TrainingData, y.vec, penalty, opt.thresh, rep(0,NCOL(y.vec)))
    W.mat <- cbind(W.mat, W.vec)
  }

  #Output: W.mat (n_features x n_penalties), weight matrix on original scale, that can be used to get predictions via X.mat %*% W.mat
  W.mat*X.mat.mean
}


LMSquareLossL2CV<-function(TrainingData, TrainingLabels, fold.vec, penalty.vec)
{
  #should use K-fold cross-validation based on the fold IDs provided in fold.vec
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  Data = cbind(TrainingData ,TrainingLabels)
  #Randomly shuffle the data
  Data<-Data[sample(nrow(Data)),]

  #TODO If invalid fold.vec .. make one for ourselves
  #TODO find folds.n from fold.vec

  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)

  W.Matrix = 0
  L2Error.Matrix = 0
  #Perform folds.n fold cross validation
  for(i in 1:folds.n)
  {
    testIndexes <- which(fold.vec==i,arr.ind=TRUE)
    testData <- Data[testIndexes, ]
    trainData <- Data[-testIndexes, ]

    #for each train/validation split, use LMSquareLossL2penalties to compute optimal L2-penalized models on the train data, then compute the validation loss of each model.
    W.Vector = LMSquareLossL2penalties(trainData[,1:DataColsEnd], trainData[,LabelCol],penalty.vec)
    W.Matrix = rbind(W.Matrix,W.Vector)

    #compute mean.validation.loss.vec, which is a vector (with n_penalties elements) of mean validation loss over all K folds.
    L2Error.Vector = Find_Wmatrix_L2Error(W.Vector,testData[,1:DataColsEnd],testData[,LabelCol],BinaryClassification)
    L2Error.Matrix = rbind(L2Error.Matrix,L2Error.Vector)
  }

  #minimize the mean validation loss to determine selected.penalty, the optimal penalty value.
  SmallestLoss = .Machine$integer.max
  Index = 0
  for(selected.steps in 1:max.iterations)
  {
    if(mean.validation.loss.vec[selected.steps] < SmallestLoss)
    {
      SmallestLoss            = mean.validation.loss.vec
      selected.penalty        = 0
    }
  }

  #finally use LMSquareLossL2penalties(penalty.vec=selected.penalty) on the whole training data set.
  LMSquareLossL2penalties(X.mat, y.vec,penalty.vec=selected.penalty)

  #Output a list with the following named elements:
  #  mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty (for plotting train/validation loss curves)
  #  weight.vec, the weight vector found by using gradient descent with selected.penalty on the whole training data set.
  #  predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
}




Logistic_Spam_Test<-function()
{
  print("Starting Spam_Test")
  Folds <- 3
  MaxNeighbors <- 30
  Local_spam<- ElemStatLearn::spam


  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])


  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])
  print(NROW(Spam))


  email = Local_spam[0,]
  email <- head(Local_spam[Local_spam$spam == 0,],MaxSample_ofType)
  #print(NROW(email))

  Cliped<-rbind(Spam,email)
  Cliped<-Cliped[sample(nrow(Cliped)),]
  DataColsStart = 0
  DataColsEnd   = NCOL(Cliped) - 1
  LabelCol      = NCOL(Cliped)
  Rows          = NROW(Cliped)

  #Create New Fold Column to hold Fold Values
  fold.vec <- Random_Folds(Rows,Folds)
  #print(Cliped)

  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Error = LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],20,0.1)
  #max.iterations = 10
  #LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol], fold.vec,Folds,30)

  Normalized_TrainingData <- as.matrix(NormalizeMatrix(as.matrix(Cliped[,DataColsStart:DataColsEnd])))
  #print(Error)
  #LMSquareLossL2(Normalized_TrainingData,  Cliped[,LabelCol], .5, 2, array(rep(0,NCOL(Normalized_TrainingData)),dim=c(1,NCOL(Normalized_TrainingData))))


}
Logistic_Spam_Test()
#LMSquareLossL2(Normalized_TrainingData,  Cliped[,LabelCol], .5, 2, rep(0,NCOL(Normalized_TrainingData)))





