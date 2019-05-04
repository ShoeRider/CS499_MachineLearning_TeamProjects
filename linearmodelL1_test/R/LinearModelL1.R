print(getwd())
source("R/General.R")
source("tests/testthat/Prep_Libraries.R")



LinearModel.Loss.Gradient<-function(TrainingData,TrainingLabels,W.Vector)
{
  #t(X.mat) %*% (as.numeric(X.mat %*% W.Vec) - y.vec) / nrow(X.mat)

  TrainingData  = data.matrix(TrainingData)
  TrainingLabels = data.matrix(TrainingLabels)

  Bias    =  data.matrix(W.Vector[1])
  Weights = data.matrix(W.Vector[2:length(W.Vector)])

  #print(dim(data.matrix(W.Vector)))
  #print(dim(Bias))
  #print(dim(Weights))

  #Gradient = 2*sum(W.Vector*TrainingData - TrainingLabels)*TrainingLabels
  Y.hat = TrainingData %*% (Weights)
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

sign <- function(x)
  {
  x[which(x>0)]=-1
  x[which(x==0)]=0
  x[which(x<0)]=1
  return(x)
}

sigmoid <- function(x)
{
  return(1 / 1 + exp(-x))
}
positive.Clip <- function(x)
{
  ifelse(x<0, 0, x)
}

soft <- function(x, lambda)
{
  sign(x) * positive.Clip(abs(x)-lambda)
}



LinearModel.L1.Gradient<-function(W.Vector,penalty,Lambda)
{
  #soft(W.Vector,penalty)
  penalty * soft(W.Vector,Lambda)
}



LinearModel.Gradient<-function(TrainingData,TrainingLabels,W.Vector,penalty,Lambda)
{
  L1Gradient = LinearModel.L1.Gradient(W.Vector,penalty,Lambda) + LinearModel.Loss.Gradient(TrainingData,TrainingLabels,W.Vector)
  #print((L1Gradient))
  #LinearModel.Loss.Gradient(TrainingData,TrainingLabels,W.Vector)
}



Find_WVector_MeanLoss<-function(TestingData,TestingLables,W.Vector,BinaryClassification)
{
  #print(dim(W.Matrix))
  W.Matrix = data.matrix(W.Vector)

  #print("finding L2Error")
  TestingData  = data.matrix(TestingData)
  TestingLables = data.matrix(TestingLables)
  Bias    = data.matrix(W.Matrix[1,])
  Weights = data.matrix(W.Matrix[2:NROW(W.Vector),])


  #print(dim(Bias))
  #print(dim(Weights))
  #print(dim(TestingData))


  Y.hat = as.matrix(TestingData %*% Weights)
  #print(dim(Y.hat))

  Yb.hat = 0
  for(X in 2:NROW(Y.hat))
  {
    Yb.hat <- rbind(Yb.hat,Y.hat[X,]+t(Bias))
  }

  #print(BinaryClassification)
  if(BinaryClassification)
  {
    Error_Vector = ifelse(Yb.hat>0.5,1,0) != as.integer(TestingLables)
    #Error_Vector = ifelse(Yb.hat>0.5,1,0) != as.integer(TestingLables)
  }
  else
  {
    Error_Vector = ((Yb.hat) - as.integer(TestingLables))**2
  }
  #0.5 * mean((as.numeric(X.mat %*% x) - y.vec)^2)

  #print(dim(Yb.hat))
  #print(dim(TestingLables))
  #print(Yb.hat-as.integer(TestingLables))

  #print()

  L1Error = as.double(colMeans(Error_Vector))
}


Find_WMatrix_MeanLoss<-function(TestingData,TestingLables,W.Matrix,BinaryClassification)
{
  print(nrow(W.Matrix))
  VectorLoss = Find_WVector_MeanLoss(TestingData,TestingLables,W.Matrix[2,],BinaryClassification)
  for (X in 2:nrow(W.Matrix))
  {
    VectorLoss <- c(VectorLoss,Find_WVector_MeanLoss(TestingData,TestingLables,W.Matrix[X,],BinaryClassification))
  }
  return(VectorLoss)
}



LinearModel.L1.Penalty <- function(W.Matrix,penalty)
{
  if(FALSE)
  {
    print("Check this out ")
    print(as.double(sum(abs(W.Matrix))))
  }
  return(as.double(penalty * sum(abs(W.Matrix))))
}


LinearModel.L1.Cost <- function(TestingData,TestingLables,W.Matrix,penalty,BinaryClassification)
{

  W.Matrix   = data.matrix(W.Matrix)
  Loss       = Find_Wmatrix_MeanLoss(TestingData,TestingLables,W.Matrix,BinaryClassification)
  L1_Penalty = LinearModel.L1.Penalty(W.Matrix,penalty)

  if(FALSE)
  {
    print("W.Matrix")
    print(dim(W.Matrix))
    print("Loss")
    print(Loss)
    print("L1_Penalty")
    print(L1_Penalty)
  }
  return (L1_Penalty+Loss)
}








#X.mat (feature matrix, n_train x n_features), y.vec (label vector, n_train x 1), max.iterations (int scalar > 1), step.size.
#'Linear Models SquareLossIterations
#'
#' Makes iterative steps using gradient decent to find a solution to the Linear Models problem
#'
#'@param Normalized_TrainingData numeric imput feature matrix [n x p]
#'@param TrainingLabels numberic input label vector [n]
#'either all 0/1 for binary classification or other real numbers for regression
#'@param Iterations integer that determines the number of steps taken to find the optimal
#'@param StepSize.Scalar scalar integer, determines the size of each step.
#'
#'@return list of:
#'    L1Error.vector = L1Error.vector,
#'    selected.steps = selected.steps,
#'    weight.mat = W.Matrix,
#'    weight.vec = as.matrix(W.Matrix[selected.steps,])
#'    W.Matrix:  W.Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
#'
#'@export
#'
#'@examples
#'  Spam<-Prep_Spam()
#'  folds.n = 4L
#'  Scalar.Step = 0.4
#'  max.iterations = 3000L
#'  fold.vec = as.double(sample(1:(4L),Spam$n_Elements,replace=T))
#'  Scaled.Train  = scale(Spam$TrainingData)
#'  Initial.Vector <- array(as.matrix(rep(0,NCOL(Scaled.Train)+1)),dim=c(1,NCOL(Scaled.Train)+1))
#'  y.vec <- as.numeric(Spam$TrainingLabels)
#'
#'  W.Matrix = LinearModelL1(Scaled.Train,y.vec,0.5,1200,Initial.Vector,1,max.iterations=50)
LinearModelL1<-function(Normalized_TrainingData,TrainingLabels,penalty,opt.thresh,Initial.Vector,StepSize.Scalar,max.iterations=5000,  Lambda = 10)
{

  TrainingLabels <- as.matrix(TrainingLabels)
  Normalized_TrainingData <- as.matrix(Normalized_TrainingData)
  #print("Starting LMSquareLossL2")
  BinaryClassification <- all(ifelse(TrainingLabels %in% c(0, 1), TRUE, FALSE))
  if (BinaryClassification)
  {
    TrainingLabels <- ifelse(TrainingLabels == 0, -1, 1)
  }

  if(FALSE)
  {
    print("Binary?")
    print(BinaryClassification)
    print("MEan y")
    print((TrainingLabels))
    print((TrainingLabels[1000:length(TrainingLabels)]))
    print((TrainingLabels[2000:length(TrainingLabels)]))
    print((TrainingLabels[3000:length(TrainingLabels)]))
  }

  #Itterate till Cur.thresh < opt.thresh of times
  validation.loss = 0
  Iteration = 0
  n.features <- ncol(Normalized_TrainingData)   # p
  n.trains <- nrow(Normalized_TrainingData)  # n
  #initial Matrix for iteration 1
  #print("Initial matrix")
  #print(dim(as.matrix(initial.weight.vec)))
  #print(t(as.matrix(initial.weight.vec)))

  #W.Matrix = array(as.matrix(Initial.Vector),dim=c(1,NCOL(Initial.Vector)))
  W.Matrix = Initial.Vector
  #print("W.matrix")
  #print((W.Matrix))
  #print(dim(W.Matrix))

  Error = 0

  #print(dim(W.Matrix))
  L1_norm.val = 100000
  L1Error.vector <- Find_WVector_MeanLoss(Normalized_TrainingData, TrainingLabels,W.Matrix[1,],BinaryClassification)
  #Itterate # of times
  #---------------------------------------------------------------------------------------------
  #for(Iteration in 1:100)
  Iteration = 0
  w.gradient.vec = .Machine$integer.max
  #while((L1_norm.val > opt.thresh)&&(Iteration < max.iterations))
  while(all(Iteration < max.iterations))
  {
    Iteration = 1+Iteration

    #Regularized.Gradient = rep(0,NCOL(Normalized_TrainingData)+1)
    #Regularized.Gradient = LinearModel.Loss.Gradient(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,])
    Regularized.Gradient = -StepSize.Scalar*LinearModel.Gradient(Normalized_TrainingData,TrainingLabels,W.Matrix[Iteration,],penalty,Lambda)

    W.Matrix <- rbind(W.Matrix, W.Matrix[Iteration,]+(Regularized.Gradient))

    #Find_WVector_MeanLoss(TestingData,TestingLables,W.Matrix,BinaryClassification)
    L1_norm.val <- Find_WVector_MeanLoss(Normalized_TrainingData, TrainingLabels,W.Matrix[Iteration,],BinaryClassification)
    #print(L1_norm.val)
    L1Error.vector<-rbind(L1Error.vector,L1_norm.val)


    #w.gradient.vec <-
    #  t(Normalized_TrainingData) %*% (TrainingLabels / (1 + exp(TrainingLabels * (
    #    Normalized_TrainingData %*% (W.Matrix[Iteration,]) + rep(1,n.trains)))))
  }
  if(FALSE)
  {
    print("dim w.matrix")
    print(W.Matrix)
    print(dim(W.Matrix))
  }
  if(FALSE)
  {
    print("dim L1Error.vector")
    print(L1Error.vector)
    print(dim(L1Error.vector))
  }

  SmallestLoss = .Machine$integer.max
  selected.steps = 0
  for(Index in 1:Iteration)
  {

    if(L1Error.vector[Index] < SmallestLoss)
    {
      SmallestLoss    = L1Error.vector[Index]
      selected.steps  = Index
    }
  }

  #print("selected vector")
  #print(length(as.double(W.Matrix[selected.steps,])))

  #Returns: Optimal weight vector (with p+1 elements, first element is the bias/intercept b) for the given penalty parameter.
  #return()
  result.list <- list(
    L1Error.vector = L1Error.vector,
    selected.steps = selected.steps,
    weight.mat = W.Matrix,
    weight.vec = as.matrix(W.Matrix[selected.steps,])
  )
  #return(W.Matrix)
}

Test_LinearL1<-function()
{

  Spam<-Prep_Spam()

  print("Spam")

  print(dim(Spam$TrainingData))
  print(dim(Spam$TrainingLabels))
  #(Spam$TrainingData)
  #(Spam$TrainingLabels)


  folds.n = 4L
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

  List = LinearModelL1(Scaled.Train,y.vec,0.5,1200,Initial.Vector,.5,max.iterations=50)
  #w.vec = LinearModelL1(Scaled.Train,as.numeric(Spam$TrainingLabels),1,1200,Initial.Vector,5,max.iterations=1000)

  print(List$weight.mat)
}
#Test_LinearL1()
