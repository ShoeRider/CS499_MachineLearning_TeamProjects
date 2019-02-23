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
  return((Matrix - mean)/sd)
}



#L1 abs(Y.hat - Y.truth)
#L2 (Y.hat - Y.truth)**2


LMSquare_Prediction<-function(TrainingData,TrainingLabels,W.mat)
{
  Y.Hat = (TrainingData*W.mat)
}

LMSquare_L2Error<-function(TrainingData,TrainingLabels,W.Vector)
{
  Y.hat = data.matrix(TrainingData) %*% data.matrix(W.Vector)

  L2 = sum((Y.hat - TrainingLabels)**2)
}

LMSquare_Gradient<-function(TrainingData,TrainingLabels,W.Vector)
{
  #print(dim(data.matrix(W.Vector)))
  #print(dim(data.matrix(TrainingData)))

  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
  Y.hat = data.matrix(TrainingData) %*% data.matrix(W.Vector)

  SquaredNorm = (Y.hat - TrainingLabels)

  # with a scalar of 2 , but its not needed becasue This vector will be normalized
  Gradient = (t(TrainingData) %*% SquaredNorm)
}


#Find_Wmatrix_L2Error
#Takes: (W.Matrix,TestingData.normalized,TestingLables,BinaryClassification)
#Note:
#Calculates the ((Y.hat) - (Y.Truth))**2
#With exception to BinaryClassification models
# were Y.Hat is determined to be 0,1 and compaired to Y.Truth
# Error Is either 0 or 1.
#returns: vector of fitness of every W.Matrix Row
Find_Wmatrix_MeanL2Error<-function(TestingData,TestingLables,W.Matrix)
{
  #print("W.Matrix")
  #print(dim(W.Matrix))

  L2Error.matrix = 0
  Y.hat = data.matrix(TestingData) %*% data.matrix(W.Matrix)
  Error_Vector = (Y.hat - TestingLables)**2

  #if(BinaryClassification)
  #{
  #  Average_Y.Hat = data.matrix(colMeans(ifelse(Y.hat>0.5,1,0)))
  #  Error_Vector = as.integer(Average_Y.Hat != as.integer(TestingLables))
  #Error_Vector = ((Y.hat) - as.integer( TestingLables))**2
  #}else{
  #}
  #print(dim(Error_Vector))
  L2Error.Matrix = (as.matrix(colMeans(Error_Vector)))
}


#X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
LMSquareLossIterations<-function(TrainingData, TrainingLabels,Iterations,StepSize.Scalar)
{
  #Housekeeping, and finding general info
  TrainingData <- data.matrix(TrainingData)
  mean = sum(TrainingData)/NROW(TrainingData)
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  # the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat
  # during gradient descent, it will not converge – numerical instability).
  TrainingData.colMeans = colMeans(TrainingData)
  Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))
  BackTrack_Test <-mean*Normalized_TrainingData

print(TrainingData)
print(Normalized_TrainingData)

  #initial Matrix for iteration 1
  W.mat = array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))


  #print(W.mat)
  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {
    print(LMSquare_L2Error(Normalized_TrainingData,TrainingLabels,W.mat[Iteration,]))
    #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.mat[Iteration,])


    NormalizedGradient <- data.matrix(NormalizeMatrix(Gradient))


    W.mat <- rbind(W.mat, W.mat[Iteration,]-t(NormalizedGradient))
    #print(sum(NormalizedGradient))

    #print(Find_Wmatrix_MeanL2Error(New.W.mat,Normalized_TrainingData,TrainingLabels))
  }
  print("L2 Error")
  #print(Find_Wmatrix_MeanL2Error(W.mat,Normalized_TrainingData,TrainingLabels))

  #I think this is how to renormalize the W.Matrix, I did a short test, but dont know
  #how valid/ it was
  #error = Find_Wmatrix_R2Error(mean*W.mat,TrainingData,TrainingLabels,BinaryClassification)
  #print(sum(error))
  #error = Find_Wmatrix_R2Error(W.mat,Normalized_TrainingData,TrainingLabels,BinaryClassification)
  #print(sum(error))

  # TODO return W.mat on the original scale
  W.mat

  # optimize the mean loss (not the total loss).
}




#LMSquareLossEarlyStoppingCV
#Takes: (TrainingData, TrainingLabels, fold.vec,folds.n=4,max.iterations)
#
LMSquareLossEarlyStoppingCV<-function(TrainingData, TrainingLabels, fold.vec,folds.n=4,max.iterations)
{
  step.size = 1
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
  W.Fold.Matrix = 0
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

    Error            =  Find_Wmatrix_L2Error(W.Matrix,test.Data,test.Labels,BinaryClassification)
    L2Error.Matrix   <- cbind(L2Error.Matrix,Error)

  }
  #on the train data, then compute the validation loss of each model.
  #print(L2Error.Matrix)
  #compute mean.validation.loss.vec, which is a vector (with max.iterations elements)
  # of mean validation loss over all K folds.

  #print(L2Error.Matrix)

  #mean.validation.loss.vec = rowMeans(L2Error.Matrix)

  #minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
  #SmallestLoss = .Machine$integer.max
  #selected.Index = 0
  #for(Index in 1:max.iterations)
  #{
  #  if(mean.validation.loss.vec[selected.steps] < SmallestLoss)
  #  {
  #    SmallestLoss = mean.validation.loss.vec
  #    selected.Index        = 0
  #  }
  #}

  #finally use LMLinearSquareLossIterations(max.iterations=selected.steps) on the whole training data set.
  #MainWeight.Vector = LMSquareLossIterations(TrainingData, TrainingLabels,Iterations,StepSize.Scalar)

  #ReturnList = 0
  #return a list with:
  #mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
  #selected.steps
  #weight.vec, the weight vector found by using gradient descent with selected.steps on the whole training data set.
  #predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
}

LinearSquare_Spam_Test<-function()
{
  print("Starting Spam_Test")
  Folds <- 4
  #MaxNeighbors <- 30
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
  #fold.vec <- cut(seq(1,Rows,breaks=4,labels=FALSE))
  #folds    <- cut(seq(1,nrow(yourData)),breaks=Folds,labels=FALSE)
  fold.vec <- Random_Folds(Rows,Folds)
  #print(Cliped)

  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],10,2)

  #max.Iterations = 2
  #LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],fold.vec,Folds,max.Iterations)
}
LinearSquare_Spam_Test()


#LMSquareLossL2
#--------------------------------------------------------------------------------------------------------------
#Finds the optimal weight vector that minimizes the following cost function:
# Sum   (L[w^T x_i, y_i] + penalty * ||w|| )
#i=1^n

#L<-SquareLoss()

#penalty  (non-negative numeric scalar)

#Gradient cliping ?
#opt.thresh (positive numeric scalar)
#opt.thresh should be a threshold on the L1-norm (sum of absolute values) of the gradient.
#I will test your code to make sure that the L1-norm of the gradient of your solution is less than opt.thresh.

LMSquareLossL2<-function(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)
{
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)


  #initial Matrix for iteration 1
  W.mat = array(initial.weight.vec,dim=c(1,NCOL(TrainingData)))

  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {
    #ind the optimal weight vector that minimizes the following cost function:
    # Sigma   L[w^T x_i, y_i] + penalty * ||w||
    #i=1^n
    #where L is either the logistic or square loss.
    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.mat[,Iteration],0)
    RegularizedGradient <- Gradient + penalty*(colSums(W.mat**2))**(1/2)
    #Gradient clip

    apply(RegularizedGradient, function(y) min(max(y,(-opt.thresh)),(opt.thresh)))
    W.mat <- cbind(W.mat, W.mat[,Iteration]+RegularizedGradient)
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






