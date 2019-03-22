source("R/General.R")



#' LMLogistic_Gradient
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'@param W.Vector Weight values representing the Linear funciton [p+1],
#'
#' @return returns the gradient of the W.Vector, in respect to the TrainingData, and TrainingLabels
#' Uses the following equation to find the gradients:
#' #Weight.Gradient = sum(W.Vector*TrainingData - TrainingLabels)*TrainingLabels
#' #BiasGradient = sum(Yb.hat - TrainingLabels)
#'
#' @export
#'
#' @examples
LMLogistic_Gradient<-function(TrainingData,TrainingLabels,W.Vector)
{
  print("LMLogistic_Gradient")
  TrainingData  = data.matrix(TrainingData)
  TrainingLabels = data.matrix(TrainingLabels)

  Bias    =  data.matrix(W.Vector[1])
  Weights = data.matrix(W.Vector[2:length(W.Vector)])

  print(dim(Weights))
  print(dim(TrainingLabels))
  print(dim(TrainingData))

  #Gradient = 2*sum(W.Vector*TrainingData - TrainingLabels)*TrainingLabels
  Y.hat = 1/(1+exp(-(TrainingData %*% (Weights))))


  W.gradient.vec = -t(TrainingData) %*% (TrainingLabels / (1 + exp(TrainingLabels * (TrainingData %*% W.Vector + rep(1,n.train) * Bias ))))
  # Calculate L(beta)'

  #beta.gradient <-
  #  -sum(y.vec / (1 + exp(y.vec * (
  #    X.scaled.mat %*% W.temp.vec + rep(1,n.train) * beta.temp
  #  ))))/n.train


GradientY.hat = as.matrix((exp(-(TrainingData %*% (Weights))))/(1+exp(-(TrainingData %*% Weights)))**2)
print(dim(GradientY.hat))
stop("YO Bro 3 ")


  FullGradient <- as.matrix(rbind(BiasGradient,(as.matrix(rowMeans(Gradient)))))
  FullGradient <- FullGradient/sum(FullGradient)
  #stop("O NO2")
  return <- t(FullGradient)
}
OG_LMLogistic_Gradient<-function(TrainingData,TrainingLabels,W.Vector)
{
  print("LMLogistic_Gradient")
  TrainingData  = data.matrix(TrainingData)
  TrainingLabels = data.matrix(TrainingLabels)

  Bias    =  data.matrix(W.Vector[1])
  Weights = data.matrix(W.Vector[2:length(W.Vector)])

  #print(dim(data.matrix(W.Vector)))
  print(dim(TrainingLabels))
  print(dim(TrainingData))

  #Gradient = 2*sum(W.Vector*TrainingData - TrainingLabels)*TrainingLabels
  #%*% t(data.matrix(TrainingData))
  Y.hat = data.matrix(TrainingData) %*% (Weights)
  #print("YB hat")
  Yb.hat = Y.hat + Bias[1]
  Y.hat = 1/(1+exp(-data.matrix(TrainingLabels) %*% data.matrix(W.Vector)))
  #print("YB hat")
  Yb.hat = Y.hat + Bias[1]

  Difference = (Y.hat - TrainingLabels)
  BiasGradient = sum(Yb.hat - TrainingLabels)


  # with a scalar of 2 , but its not needed becasue This vector will be normalized
  Gradient = (t(TrainingData) %*% Difference)


  FullGradient <- as.matrix(rbind(BiasGradient,Gradient))
  FullGradient <- FullGradient/sum(FullGradient)

  return <- t(FullGradient)
}

Temp_LMLogistic_Gradient<-function(TrainingData,TrainingLabels,W.Vector)
{
  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
  #Y.hat = data.matrix(TrainingData) %*% data.matrix(W.Vector)

  Y.hat = 1/(1+exp(-(TrainingData %*% (Weights))))

  SquaredNorm = (Y.hat - TrainingLabels)

  # with a scalar of 2 , but its not needed becasue This vector will be normalized
  Gradient = (t(TrainingData) %*% SquaredNorm)
}

#'LMLogistic_Gradient_L2Regularization
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem.
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'either all 0/1 for binary classification or other real numbers for regression
#'@param StepSize.Scalar scalar integer, determines the size of each step.
#'@param W.Vector Weight values representing the Logistic funciton [p+1],
#'@param Penalty Scalar that is used to control the L2 regularization operation, to turn off just use '0'(Zero)
#'
#'@return returns the gradient of the W.Vector, in respect to the TrainingData, and TrainingLabels
#'@return numeric matrix of the weight matrix at each step, from 1 to Iterations.
#'Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'@export
#'
#' @examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
LMLogistic_Gradient_L2Regularization<-function(TrainingData,TrainingLabels,W.Vector,Penalty=0)
{

  #print(dim(W.Matrix))
  W.Vector = data.matrix(W.Vector)
  #print(t(W.Vector))
  #print("finding L2Error")
  TrainingData  = data.matrix(TrainingData)
  TrainingLabels = data.matrix(TrainingLabels)
  Bias    = data.matrix(W.Vector[1])
  Weights = data.matrix(W.Vector[2:NROW(W.Vector)])


  #print("Bias:")
  #print(Bias)

  #print("Y.Hat")
  #print(dim(TrainingData))
  #print(dim(Bias))
  #print(dim(Weights))

  #Gradient = 2*sum(W.Vector*TrainingData + TrainingLabels)*TrainingLabels
  Y.hat = 1/(1+exp(-(TrainingData %*% (Weights))))
  Yb.hat = Y.hat + Bias[1]

  LogisticdNorm = (Y.hat - TrainingLabels)

  # with a scalar of 2 , but its not needed becasue This vector will be normalized
  #print((Penalty)*diag(nrow(W.Vector)))
  #print("Penalty:")
  #print(Penalty)

  #print(t(W.Vector)%*%data.matrix(W.Vector))
  #L2_W = data.matrix(t(W.Vector)%*%data.matrix(W.Vector))

  Gradient = (t(TrainingData) %*% LogisticdNorm)

  W.Vector.Distance = sum(Weights%*%t(Weights))
  W.Vector.Gradient = Gradient + (Penalty*W.Vector.Distance)

  #W.Bias.Gradient = Gradient + (Penalty*W.Vector.Distance)
  BiasGradient = sum(Yb.hat - TrainingLabels)

  #print("BiasGradient")
  #print(Bias)
  #print(BiasGradient)

  #print("Dim of L2Reg gradient")
  #print(dim(t(as.matrix(rbind(BiasGradient,0)))))


  return <- t(as.matrix(rbind(BiasGradient,W.Vector.Gradient)))
  #return <- t(as.matrix(rbind(0,W.Vector.Gradient)))
  #return <- t(as.matrix(rbind(0,W.Vector.Gradient)))
}



#Find_Wmatrix_L2Error
#Takes: (W.Matrix,TestingData.normalized,TestingLables,BinaryClassification)
#Note:
#Calculates the ((Y.hat) - (Y.Truth))**2
#With exception to BinaryClassification models
# were Y.Hat is determined to be 0,1 and compaired to Y.Truth
# Error Is either 0 or 1.
#returns: vector of fitness of every W.Matrix Row


#'Find_Wmatrix_MeanL2Error
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'either all 0/1 for binary classification or other real numbers for regression
#'@param StepSize.Scalar scalar integer, determines the size of each step.
#'@param W.Matrix Weight values representing the Logistic funciton [p+1,Iterations],
#'@param BinaryClassification binary or integer value that determines how the algorithm classifies the retulsts
#'If (BinaryClassification):
#'  Then Classification is either 0 or 1.
#'
#'@return L2Error.Matrix
#'@export
#'
#' @examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
FindLogistic_Wmatrix_MeanL2Error<-function(TestingData,TestingLables,W.Matrix,BinaryClassification)
{
  #print(dim(W.Matrix))
  W.Matrix = data.matrix(W.Matrix)

  #print("finding L2Error")
  TestingData  = data.matrix(TestingData)
  Bias    = data.matrix(W.Matrix[1,])
  Weights = data.matrix(W.Matrix[2:NROW(W.Matrix),])


  #print(dim(Bias))
  #print(dim(Weights))
  #print(dim(TestingData))

  L2Error.matrix = 0


  #Y.hat = as.matrix(TestingData %*% Weights)
  Y.hat = 1/(1+exp(-(TestingData %*% (Weights))))
  #print(dim(Y.hat))

  Yb.hat = 0
  for(X in 2:NROW(Y.hat))
  {
    Yb.hat <- rbind(Yb.hat,Y.hat[X,]+t(Bias))
  }


  if(BinaryClassification)
  {
    Error_Vector = ifelse(Yb.hat>0.5,1,0) != as.integer(TestingLables)
    #Error_Vector = ((Y.hat) - as.integer( TestingLables))**2
  }else{
    Error_Vector = ((Yb.hat) - as.integer(TestingLables))**2
  }

  L2Error.Matrix = (as.matrix(colMeans(Error_Vector)))
}



#' L1-norm
#'
#' finds the error of a Regularized(L2) Matrix
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'either all 0/1 for binary classification or other real numbers for regression
#'@param StepSize.Scalar scalar integer, determines the size of each step.
#'@param W.Matrix Weight values representing the Logistic funciton [p+1,Iterations],
#'@param BinaryClassification binary or integer value that determines how the algorithm classifies the retulsts
#'If (BinaryClassification):
#'  Then Classification is either 0 or 1.
#'
#'@return L2Error.Matrix
#'@export
#'
#' @examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
FindLogistic_Wmatrix_MeanL1Error<-function(TestingData,TestingLables,W.Matrix,BinaryClassification)
{
  #print(dim(W.Matrix))
  W.Matrix = data.matrix(W.Matrix)

  #print("finding L2Error")
  TestingData  = data.matrix(TestingData)
  TestingLables = data.matrix(TestingLables)
  Bias    = data.matrix(W.Matrix[1,])
  Weights = data.matrix(W.Matrix[2:NROW(W.Matrix),])


  #print(dim(Bias))
  #print(dim(Weights))
  #print(dim(TestingData))


  #Y.hat = as.matrix(TestingData %*% Weights)
  Y.hat = 1/(1+exp(-(TestingData %*% (Weights))))
  #print(dim(Y.hat))

  Yb.hat = 0
  for(X in 2:NROW(Y.hat))
  {
    Yb.hat <- rbind(Yb.hat,Y.hat[X,]+t(Bias))
  }

  print(BinaryClassification)
  if(BinaryClassification)
  {
    Error_Vector = ifelse(Yb.hat>0.5,1,0) != as.integer(TestingLables)
    #Error_Vector = abs((Y.hat) - as.integer(TestingLables))


  }else{
    Error_Vector = abs((Yb.hat) - as.integer(TestingLables))
  }
  #print("Yb.hat-TestingLables")
  #print(dim(Yb.hat))
  #print(dim(TestingLables))
  #print(Yb.hat-as.integer(TestingLables))

  #print()

  L1Error = (as.matrix(colMeans(Error_Vector)))
}



#X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
#'Logistic Models LogisticLossIterations
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'either all 0/1 for binary classification or other real numbers for regression
#'@param Iterations integer that determines the number of steps taken to find the optimal
#'@param StepSize.Scalar scalar integer, determines the size of each step.
#'
#'@return numeric matrix of the weight matrix at each step, from 1 to Iterations.
#'Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'@export
#'
#'@examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
LMLogisticLossIterations<-function(TrainingData, TrainingLabels,Iterations = 10,StepSize.Scalar)
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


  #print(dim(TrainingData))
  #print(dim(Normalized_TrainingData))
  #print(dim(TrainingLabels))

  #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of
  # the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat
  # during gradient descent, it will not converge – numerical instability).
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #initial Matrix for iteration 1
  W.Matrix = array(rep(0,NCOL(TrainingData)+1),dim=c(0,NCOL(TrainingData))+1)
  Error = 0
  print(dim(W.Matrix))

  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  for(Iteration in 1:Iterations)
  {
    Gradient = LMLogistic_Gradient(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,])
    StepSize.Gradient <- data.matrix(Gradient)*StepSize.Scalar

    #print("Gradient and Itteration")
    #print(dim(data.matrix(W.Matrix[Iteration,])))
    #print(dim(t(StepSize.Gradient)))
    W.Matrix <- rbind(W.Matrix, W.Matrix[Iteration,]+(StepSize.Gradient))
    #print("added to W.Matrix")

    #error of each iteration
    #print(W.Matrix[Iteration,])
    #print(Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,(W.Matrix[Iteration,]),BinaryClassification))
  }



  #print("find error")

  #Norm.Error<- Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification)
  #print(Norm.Error)
  #print(Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification))


  #DeNormalized.TrainingData<-((Normalized_TrainingData)*TrainingData.sd)+TrainingData.mean
  #*colMeans(W.Matrix)
  #-t(W.Matrix/TrainingData.sd)
  DeNormalizedWeightMatrix = (t(W.Matrix))/TrainingData.sd
  #print(DeNormalizedWeightMatrix)

  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(TrainingData,TrainingLabels,DeNormalizedWeightMatrix,BinaryClassification)
  #Norm.Error   <-Find_Wmatrix_MeanL2Error(Normalized_TrainingData,TrainingLabels,t(W.Matrix),BinaryClassification)

  #barplot(Norm.Error,main = "LM Norm.Error",xlab = "mean loss value",beside = TRUE)
  #barplot(DeNorm.Error,main = "LM DeNorm.Error:L2 spam",xlab = "mean loss value",beside = TRUE)

  return(DeNormalizedWeightMatrix)
}



#LMLogisticLossEarlyStoppingCV
#Takes: (TrainingData, TrainingLabels, fold.vec,folds.n=4,max.iterations)
#
#'Logistic Models LogisticLossIterations
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param fold.vec,
#'@param folds.n,
#'@param max.iterations integer that determines the number of steps taken to find the optimal
#'
#'@return List of :
#'
#'Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'@export
#'
#'@examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
LMLogisticLossEarlyStoppingCV<-function(TrainingData, TrainingLabels, fold.vec,folds.n=4,max.iterations=30)
{
  TrainingData <- data.matrix(TrainingData)
  TrainingLabels <- data.matrix(TrainingLabels)
  fold.vec <- data.matrix(fold.vec)

  #print(dim(TrainingLabels))
  #TrainingData <- data.matrix(TrainingData)
  TrainingData.mean <- mean(TrainingData)
  TrainingData.Sum = 0

  for( row in 1:nrow(TrainingData))
  {
    TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  }

  # get sd from the calculated sum and number of observations
  TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))
  Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))




  step.size = 0.1
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  Data = cbind(TrainingData ,TrainingLabels)
  #Randomly shuffle the data
  Data<-Data[sample(nrow(Data)),]

  #TODO If invalid fold.vec .. make one for ourselves

  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)
  W.Matrix=0
  #initialize Error Matrix:
  L2Error.Matrix = 0
  mean.validation.loss=0
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


    W.Vector =  LMLogisticLossIterations(train.Data,train.Labels,max.iterations,step.size)


    tempmatrix = (W.Vector/TrainingData.sd)


    #on the train data, then compute the validation loss of each model.
    Error            =  Find_Wmatrix_MeanL2Error(test.Data,test.Labels,W.Vector,BinaryClassification)
    L2Error.Matrix   <- rbind(L2Error.Matrix,t(Error))
  }

  #compute mean.validation.loss.vec, which is a vector (with max.iterations elements)
  # of mean validation loss over all K folds.
  L2Error.AverageVector = colMeans(L2Error.Matrix)

  #barplot(L2Error.AverageVector,main = "LM LogisticLoss:L2 spam",xlab = "mean loss value",beside = TRUE)



  #minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
  SmallestLoss = .Machine$integer.max
  selected.steps = 0
  for(Index in 1:max.iterations)
  {

    if(L2Error.AverageVector[Index] < SmallestLoss)
    {
      SmallestLoss    = L2Error.AverageVector[Index]
      selected.steps  = Index
    }
  }

  #print("Last LMLogisticLossIterations")

  #finally use LMLogisticLogisticLossIterations(max.iterations=selected.steps) on the whole training data set.
  #print(selected.steps)
  #Matrix Should Already Be normalized
  MainWeight.Matrix = LMLogisticLossIterations(TrainingData, TrainingLabels,as.integer(selected.steps),step.size)



  #return a list with:
  #mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
  #selected.steps
  #weight.vec, the weight vector found by using gradient descent with selected.steps on the whole training data set.
  #predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).

  #not sure how to return list
  ReturnList <-
    list(
      selected.steps = selected.steps,
      w.mat = MainWeight.Matrix,
      Opt.mat = MainWeight.Matrix[,selected.steps+1],
      Loss = mean.validation.loss,
      Opt.fun <-function(Feature.Matrix){return(Feature.Matrix %*% MainWeight.Vector) }
    )
}



#'LMLogisticLossL2
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem
#'
#'@param Normalized_TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param penalty
#'@param opt.thresh
#'@param initial.weight.vec
#'
#'@return numeric matrix of the weight matrix at each step, from 1 to Iterations.
#'Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'@export
#'
#'@examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
LMLogisticLossL2<-function(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)
{
  StepSize.Scalar = .01*penalty**2
  #print("Starting LMLogisticLossL2")
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)

  #Itterate till Cur.thresh < opt.thresh of times
  validation.loss = 0
  Iteration = 0

  #initial Matrix for iteration 1
  #print("Initial matrix")
  #print(dim(as.matrix(initial.weight.vec)))
  #print(t(as.matrix(initial.weight.vec)))

  W.Matrix = array(as.matrix(initial.weight.vec),dim=c(2,NCOL(initial.weight.vec)))
  #print(dim(W.Matrix))

  Error = 0

  #print(dim(W.Matrix))
  L1_norm.val = 1
  L1Error.vector = 1
  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  #for(Iteration in 1:100)
  Iteration=0
  while((L1_norm.val > opt.thresh)&&(Iteration <250))
  {
    Iteration = Iteration+1

    #print("LMLogisticLossL2 Iteration")
    #print(Iteration)


    #Gradient = LMLogistic_Gradient(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,])
    Gradient = LMLogistic_Gradient_L2Regularization(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,],penalty)
    #print(Gradient)
    Regularized.Gradient <- data.matrix(NormalizeVector(Gradient))*StepSize.Scalar
    #print(dim(Regularized.Gradient))
    W.Matrix <- rbind(W.Matrix, W.Matrix[Iteration,]+(Regularized.Gradient))

    #print("L1 Norm")
    L1_norm.val <- Find_Wmatrix_MeanL1Error(Normalized_TrainingData, TrainingLabels,W.Matrix[Iteration,],BinaryClassification)
    L1Error.vector<-rbind(L1Error.vector,L1_norm.val)
    #print(L1_norm.val)

    #print("L1_normv > opt.thresh?")
    #print(L1_normv)
    #print(opt.thresh)
    #print(L1_normv > opt.thresh)
  }


  L1Error.AverageVector = as.matrix(colMeans(t(L1Error.vector)))
  print(dim(L1Error.AverageVector))
  #barplot(L2Error.AverageVector,main = "LM LogisticLoss:L2 spam",xlab = "mean loss value",beside = TRUE)



  #minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
  SmallestLoss = .Machine$integer.max
  selected.steps = 0
  for(Index in 1:Iteration)
  {

    if(L1Error.AverageVector[Index] < SmallestLoss)
    {
      SmallestLoss    = L1Error.AverageVector[Index]
      selected.steps  = Index
    }
  }

  #print(SmallestLoss)
  #print(selected.steps)

  #print(W.Matrix)
  #print(W.Matrix[nrow(W.Matrix),])
  #Output: optimal weight vector for the given penalty parameter.

  #print(dim(W.Matrix))
  #print(L1Error.AverageVector)
  #print("Selected steps")
  #print(selected.steps)

  return(W.Matrix[selected.steps,])
}





#penalty.vec (vector of decreasing penalty values)

#'LMLogisticLossL2penalties
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param penalty.vec scalar integer, determines the size of each step.
#'
#'@return numeric matrix of the weight matrix at each step, from 1 to Iterations.
#'Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'@export
#'
#'@examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
LMLogisticLossL2penalties<-function(TrainingData, TrainingLabels,penalty.vec)
{
  TrainingLabels = as.matrix(TrainingLabels)

  #this function should begin by scaling X.mat to obtain X.scaled.mat with each column mean=0 and sd=1
  Normalized_List <- NormalizeMatrix_List(TrainingData)
  Normalized_TrainingData <- Normalized_List$NormalizedMatrix


  W.Matrix = array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))
  Optimum.W.Matrix = array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))
  #print(dim(W.Matrix))

  for(X in 1:NCOL(penalty.vec))
  {
    #print("Iteration")
    #print(X)

    return.Matrix <- LMLogisticLossL2(Normalized_TrainingData, TrainingLabels, penalty.vec[X], .1, (Optimum.W.Matrix))
    Optimum.W.Matrix <- t(return.Matrix)
    W.Matrix <- rbind(W.Matrix, (Optimum.W.Matrix))


  }

  #Output: W.mat (n_features x n_penalties), weight matrix on original scale, that can be used to get predictions via X.mat %*% W.mat
  DeNormalizedWeightMatrix = (t(W.Matrix))/Normalized_List$sd
}


#'LMLogisticLossL2 Cross Validation
#'
#' Makes iterative steps using gradient decent to find a solution to the Logistic Models problem
#'
#'@param TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param fold.vec
#'@param penalty.vec
#'
#'@return numeric matrix of the weight matrix at each step, from 1 to Iterations.
#'Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'@export
#'
#'@examples
#'## Example: 1 With The Spam DataSet:##
#'
#'## Example: 2  With The ZipTrain Dataset:##
#'
LMLogisticLossL2CV<-function(TrainingData, TrainingLabels, fold.vec, penalty.vec)
{
  #should use K-fold cross-validation based on the fold IDs provided in fold.vec
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  folds.n = max(fold.vec)
  print("folds.n:")
  print(folds.n)

  print("penalty.vec dim")
  print(dim(penalty.vec))
  print(penalty.vec)

  Data = cbind(TrainingData ,TrainingLabels)
  #Randomly shuffle the data
  Data<-Data[sample(nrow(Data)),]

  #TODO If invalid fold.vec .. make one for ourselves
  #TODO find folds.n from fold.vec

  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)


  print("Data dim")
  print(dim(Data))

  W.Matrix = t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(0,NCOL(TrainingData)+1)))

  L2TestError.Matrix = 0
  L2TrainError.Matrix = 0

  #Perform folds.n fold cross validation
  for(i in 1:(folds.n))
  {
    testIndexes <- which(fold.vec==i,arr.ind=TRUE)
    testData <- Data[testIndexes, ]
    trainData <- Data[-testIndexes, ]

    print("fold#/ Training Data/testData")
    print(i)
    print(dim(trainData))
    print(dim(testData))

    #for each train/validation split, use LMLogisticLossL2penalties to compute optimal L2-penalized models on the train data, then compute the validation loss of each model.
    W.Vector = as.matrix(LMLogisticLossL2penalties(trainData[,1:DataColsEnd], trainData[,LabelCol],penalty.vec))


    #compute mean.validation.loss.vec, which is a vector (with n_penalties elements) of mean validation loss over all K folds.
    L2TestError.Vector = Find_Wmatrix_MeanL1Error(testData[,1:DataColsEnd],testData[,LabelCol],W.Vector,BinaryClassification)
    L2TestError.Matrix = rbind(L2TestError.Matrix,t(L2TestError.Vector))

    #compute mean.validation.loss.vec, which is a vector (with n_penalties elements) of mean validation loss over all K folds.
    L2TrainError.Vector = Find_Wmatrix_MeanL1Error(trainData[,1:DataColsEnd],trainData[,LabelCol],W.Vector,BinaryClassification)
    L2TrainError.Matrix = rbind(L2TrainError.Matrix,t(L2TrainError.Vector))
  }

  print("dim L2TestError.Matrix")
  print(dim(L2TestError.Matrix))
  print("dim L2TrainError.Vector")
  print(dim(L2TrainError.Vector))

  L2TestError.AverageVector  = as.matrix(colMeans(L2TestError.Matrix))
  L2TrainError.AverageVector = as.matrix(colMeans(L2TrainError.Vector))

  print("dim L2TestError.AverageVector")
  print(dim(L2TestError.AverageVector))
  print("dim L2TrainError.AverageVector")
  print(dim(L2TrainError.AverageVector))

  #minimize the mean validation loss to determine selected.penalty, the optimal penalty value.
  #minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.



  SmallestLoss = .Machine$integer.max
  selected.steps = 0
  for(Index in 1:NCOL(penalty.vec))
  {

    if(L2TestError.AverageVector[Index] < SmallestLoss)
    {
      SmallestLoss    = L2TestError.AverageVector[Index]
      selected.steps  = Index
    }
  }

  #finally use LMLogisticLossL2penalties(penalty.vec=selected.penalty) on the whole training data set.
  weight.vec <- LMLogisticLossL2penalties(TrainingData, TrainingLabels,penalty.vec[selected.steps])

  final.loss <- Find_Wmatrix_MeanL1Error(trainData[,1:DataColsEnd],trainData[,LabelCol],W.Vector,BinaryClassification)


  #Output a list with the following named elements:
  #  mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty (for plotting train/validation loss curves)
  #  weight.vec, the weight vector found by using gradient descent with selected.penalty on the whole training data set.
  #  predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
  ReturnList <-
    list(
      mean.validation.loss = L2TestError.AverageVector,
      mean.train.loss.vec  = L2TrainError.AverageVector,
      penalty.vec          = penalty.vec,
      selected.penalty     = penalty.vec[selected.steps],
      final.loss           = final.loss,
      weight.vec           = weight.vec,
      predict<-function(testX.mat){return(testX.mat %*% weight.vec) }
    )
}







