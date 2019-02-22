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
  return<-Matrix / (colSums(Matrix**2))**(1/2)
}

NormalizeVector<-function(Vector)
{
  return<- Vector /sum(Vector)
}


LMSquare_Prediction<-function(TrainingData,TrainingLabels,W.mat)
{
  Matrix = (TrainingData*W.mat)

  Error = (TrainingLabels - Matrix)**2
}

LMSquare_Gradient<-function(TrainingData,TrainingLabels,W.Vector,Bias)
{
  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels

  Y.hat = data.matrix(TrainingData) %*% data.matrix(W.Vector)

  SquaredNorm = (Y.hat - TrainingLabels)

  # with a scalar of 2 , but its not needed becasue This vector will be normalized

  Gradient = (t(TrainingData) %*% SquaredNorm)
}
#X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
LMSquareLossIterations<-function(TrainingData, TrainingLabels,Iterations,StepSize.Scalar)
{
  TrainingData <- data.matrix(TrainingData)
  mean = sum(TrainingData)/NROW(TrainingData)
  #print("Mean")
  #print(mean)
  TrainingData.colMeans = colMeans(TrainingData)
  Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))
  BackTrack_Test <-mean*Normalized_TrainingData
  #print(BackTrack_Test)
  #print(TrainingData)
  #print(isSymmetric(BackTrack_Test,TrainingData))

  #TrainingLabels <- data.matrix(TrainingLabels)

  #print("Starting LMSquareLossIterations")
  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  # the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat
  # during gradient descent, it will not converge – numerical instability).
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #initial Matrix for iteration 1
  W.mat = 0

  #First iteration of the gradient steping process:
  #---------------------------------------------------------------------------------------------
  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
  Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,rep(0,NCOL(TrainingData)),0)
  NormalizedGradient <- data.matrix(NormalizeVector(Gradient))*StepSize.Scalar
  print(sum(NormalizedGradient))
  W.mat <- cbind(W.mat, W.mat+NormalizedGradient)



  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {
    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.mat[,Iteration],0)
    NormalizedGradient <- data.matrix(NormalizeVector(Gradient))*StepSize.Scalar
    W.mat <- cbind(W.mat, W.mat[,Iteration]+NormalizedGradient)
  }
  #print(W.mat)
  #print(L2Error.matrix)
  # TODO return W.mat on the original scale

  #I think this is how to renormalize the W.Matrix, I did a short test, but dont know
  #how valid/ it was
  #error = Find_Wmatrix_R2Error(mean*W.mat,TrainingData,TrainingLabels,BinaryClassification)
  #print(sum(error))
  #error = Find_Wmatrix_R2Error(W.mat,Normalized_TrainingData,TrainingLabels,BinaryClassification)
  #print(sum(error))

  W.mat*mean

  # optimize the mean loss (not the total loss).
}



Find_Wmatrix_R2Error<-function(W.Matrix,TestingData.normalized,TestingLables,BinaryClassification)
{
  L2Error.matrix = 0
  print(dim(W.Matrix))
  for(Iteration in 1:NROW(W.Matrix))
  {
    Y.hat = data.matrix(TestingData.normalized) %*% data.matrix(W.Matrix[,Iteration])
    if(BinaryClassification)
    {
      Error_Vector = ifelse(Y.hat>0.5,1,0) != as.integer(TestingLables)
      #Error_Vector = ((Y.hat) - as.integer( TestingLables))**2
    }else{
      Error_Vector = ((Y.hat) - as.integer( TestingLables))**2
    }
    #print(Error_Vector)

    L2Error_Vector = (Error_Vector)
    ErrorSum = sum(L2Error_Vector)
    L2Error.matrix <- rbind(L2Error.matrix, ErrorSum)
  }
  L2Error.matrix
}


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

  #Perform 10 fold cross validation
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
    #on the train data, then compute the validation loss of each model.
    W.Matrix = LMSquareLossIterations(train.Data,train.Labels,max.iterations,step.size)
    E.Matrix = Find_Wmatrix_R2Error(W.Matrix,test.Data,test.Labels,BinaryClassification)
  }
  #compute mean.validation.loss.vec, which is a vector (with max.iterations elements)
  # of mean validation loss over all K folds.
  #W.Matrix
  #colMean(E.Matrix)
  ReturnList = 0
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
  LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],1000,2)

  #max.Iterations = 100
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

LMSquareLossL2<-function(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec.)
{
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)


  #initial Matrix for iteration 1
  W.mat = 0

  #First iteration of the gradient steping process:
  #---------------------------------------------------------------------------------------------
  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
  Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,initial.weight.vec.,0)
  NormalizedGradient <- data.matrix(NormalizeVector(Gradient)) + penalty*(colSums(W.mat**2))**(1/2)
  
  print(sum(NormalizedGradient))
  W.mat <- cbind(W.mat, W.mat+NormalizedGradient)


  #>>>


  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {
    Gradient = LMSquare_Gradient(Normalized_TrainingData,TrainingLabels,W.mat[,Iteration],0)
    NormalizedGradient <- data.matrix(NormalizeVector(Gradient)) + penalty*(colSums(W.mat**2))**(1/2)
    #Gradient clip
    W.mat <- cbind(W.mat, W.mat[,Iteration]+NormalizedGradient)
  }
  #print(W.mat)
  #print(L2Error.matrix)
  # TODO return W.mat on the original scale

  #I think this is how to renormalize the W.Matrix, I did a short test, but dont know
  #how valid/ it was
  #error = Find_Wmatrix_R2Error(mean*W.mat,TrainingData,TrainingLabels,BinaryClassification)
  #print(sum(error))
  #error = Find_Wmatrix_R2Error(W.mat,Normalized_TrainingData,TrainingLabels,BinaryClassification)
  #print(sum(error))


  #Output: optimal weight vector for the given penalty parameter.
  W.mat
}

#penalty.vec (vector of decreasing penalty values)
LMSquareLossL2penalties<-function(X.mat, y.vec,penalty.vec)
{
  #this function should begin by scaling X.mat to obtain X.scaled.mat with each column mean=0 and sd=1

  #it should then loop over penalty values, calling LMSquareLossL2 to get the optimal weight vector for each.

  #Output: W.mat (n_features x n_penalties), weight matrix on original scale, that can be used to get predictions via X.mat %*% W.mat
}


LMSquareLossL2CV<-function(TrainingData, TrainingLabels, fold.vec, penalty.vec)
{
  #should use K-fold cross-validation based on the fold IDs provided in fold.vec
  #step.size = 1
  #BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #Data = cbind(TrainingData ,TrainingLabels)
  #Randomly shuffle the data
  #Data<-Data[sample(nrow(Data)),]

  #TODO If invalid fold.vec .. make one for ourselves

  #DataColsStart = 0
  #DataColsEnd   = NCOL(Data) - 1
  #LabelCol      = NCOL(Data)
  #Rows          = NROW(Data)

  #Perform 10 fold cross validation
  #for(i in 1:folds.n)
  #{
  #}
  #for each train/validation split, use LMSquareLossL2penalties to compute optimal L2-penalized models on the train data, then compute the validation loss of each model.

  #compute mean.validation.loss.vec, which is a vector (with n_penalties elements) of mean validation loss over all K folds.

  #minimize the mean validation loss to determine selected.penalty, the optimal penalty value.

  #finally use LMSquareLossL2penalties(penalty.vec=selected.penalty) on the whole training data set.


  #Output a list with the following named elements:
  #  mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty (for plotting train/validation loss curves)
  #weight.vec, the weight vector found by using gradient descent with selected.penalty on the whole training data set.
  #predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
}






