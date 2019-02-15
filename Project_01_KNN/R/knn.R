#'K Nearest Neighbor Algorithm
#'
#' Wraps around c++ code to call it using r
#'
#'@param x.mat numeric imput feature matrix [n x p]
#'@param y.vec numberic input label vector [n], 
#'either all 0/1 for binary classification or other real numbers for regression
#'@param testx.vec numberic test festure vector [p]
#'@param max.neighbors scalar integer, max number of neighbors
#'
#'@return numeric vector of size max.neighbors, predictions from 1 to max.neighbors
#'@export
#'
#'@examples
#'data(zip.train, packages= "ElemStatLearn")
#'i01 <- which(zip.train[,1] %in% c(0,1))
#'train.i <- i01[1:5]
#'test.i <- i01[6]
#'x <- zip.train[train.i,-1]
#'y <- zip.train[train.i, 1]
#'testx <- zip.train[test.i, -1]
#'knn(x,y,testx,3)
#'zip.train[test.i,1]
#'
knn <- function(x.mat, y.vec, testx.vec, max.neighbors)
{
  
  result.list <- .C("knn_interface", as.double(x.mat), as.double(y.vec),
                    as.double(testx.vec),as.integer(nrow(x.mat)),
                    as.integer(ncol(x.mat)),as.integer(max.neighbors),
                    predictions=double(max.neighbors),PACKAGE="nearestNeighbors")
}


#TODO: 
# Create even distribution version
# Rename function to something like: KNNLearnCV.Create.Fold.Vec
Random_Folds <- function(Size, Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}

KNNLearnCV.Algorithm<-function(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  Data = cbind(X.mat ,Y.vec)
  
  DataColsStart = 0
  DataColsEnd   = length(Data[1,]) - 1
  LabelCol      = length(Data[1,])
  Rows          = length(Data[,1])
  
  
  #to Create Rows of indexes
  #fold.vec <- cut(seq(1,nrow(Data)),breaks=n.folds,labels=FALSE)
  #fold.vec <- Random_Folds(length(Y.vec),n.folds)
  #print(fold.vec)
  
  Error.matrix = matrix(ncol=LabelCol)
  #print(folds.vec)
  #loop over (n.folds)folds validation 
  for(i in 1:n.folds){
    #Segement your data by fold using the which() function 
    testIndexes <- which(fold.vec==i,arr.ind=TRUE)
    #print(testIndexes)
    testData  <- Data[testIndexes, ]
    trainData <- Data[-testIndexes, ]
    
    #Separate InputData and InputLabels
    test.Data    <- testData[,DataColsStart:DataColsEnd]
    test.Labels  <- testData[,LabelCol]
    
    train.Data   <- trainData[,DataColsStart:DataColsEnd]
    train.Labels <- trainData[,LabelCol]
    
    #preform KNN Function call, and recieve ?? back 
    Error.Vector = abs(knn(train.Data, train.Labels, test.Data, max.neighbors) - test.Labels)
    Error.matrix = rbind(Error.matrix,Error.Vector)
  }
  
  
  #return a list with the following named elements:
  #  X.mat, y.vec: training data.
  #  train.loss.mat, validation.loss.mat (matrices of loss values for each fold and number of neighbors).
  #  train.loss.vec, validation.loss.vec (vectors with max.neighbors elements: mean loss over all folds).
  #  selected.neighbors (number of neighbors selected by minimizing the mean validation loss).
  #  predict(testX.mat), a function that takes a matrix of inputs/features and returns a vector of predictions. It should check the type/dimension of testX.mat and stop() with an informative error message if there are any issues.
  
}

#KNNLearnCV, takes in ... and uses the folding technique to cross validate the best possible 
# hyperparameter k, The Number of Nearest neighbors compaired to when predicting a value.
KNNLearnCV<-function(TrainingInput, TrainingLabel, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  if(n.folds <= 0){
    print("( Parameter(4)) 'n.folds' contains a non valid answer(n.folds <= 0), and should contain the number of Folds to preform")
    print("As default, n.folds will be set to 5")
    n.folds = 5
  }
  
  
  if(max.neighbors <= 0)
  {
    print("Warning: (Parameter(3)) 'max.neighbors' contains a non valid answer(max.neighbors <= 0), and should contain the hyperparameter K, the max hyperparameter to test")
    print("As default, max.neighbors will be set to 30")
    max.neighbors = 30
  }
  else if(max.neighbors > length(TrainingInput[,1]))
  {
    print(sprintf("max.neighbors:%d , TrainingInput[,1]:%d",max.neighbors,length(TrainingInput[,1])))
    print("Warning: (Parameter(3)) 'max.neighbors' is less than the number of instances found in the training data (max.neighbors > length(TrainingInput)),")
    print("in protection the default, max.neighbors will be set to the length(TrainingInput) ")
    max.neighbors = length(TrainingInput[,1])
  }
  

  
  #check if fold.vec is correct, and meets proper requirements:
  if(is.null(fold.vec))
  {
    print("Warning: ( Parameter(3)) 'fold.vec' contained a NULL value")
    print(sprintf("This is probably due to directly passing in a NULL value, a Random_Folds array will be created based on the n.folds value:%d",n.folds))
    fold.vec <-Random_Folds(length(TrainingLabel),n.folds)
  }
  if(!(length(dim(fold.vec)) %in% c(0,1)))
  {
    print("Warning: ( Parameter(3)) (length(dim(fold.vec)) is not 0 or 1!!")
    print("a new set fold.vec will be created to produce a result")
    fold.vec <-Random_Folds(length(TrainingInput[,1]),n.folds)
  }
  if(!(as.character(typeof(fold.vec)) %in% c("list","integer","double")))
  {
    print(sprintf("Warning: ( Parameter(3)) 'fold.vec' contains an element not a list, integer, or double, but as a typeof: %s ",typeof(fold.vec)))
    print(sprintf("Try Passing Parameter(3) like so (..,list(Parameter(3)),..) 'fold.vec' contains an element not a list or double "))
    print("a new set fold.vec will be created to produce a result")
    fold.vec <-Random_Folds(Training.Instances,n.folds)
  }
  if((length(fold.vec) == 0))
  {
    print("Warning: ( Parameter(3)) 'fold.vec'  contains a length of 0, or no elements")
    print("a new set fold.vec will be created to produce a result")
    fold.vec <-Random_Folds(Training.Instances,n.folds)
  }
  
  
  
  if(length(TrainingInput) == 0){
    stop("Error: (Parameter(0)) 'Training Data' contains a length of 0, or no elements should Contain a Matrix")
  }
  if(length(TrainingLabel) == 0){
    stop("Error: (Parameter(1)) 'Training Labels' contains a length of 0, or no elements should Contain a list of Tablels")
  }
  #print_TrainingData(TrainingInput, TrainingLabel, max.neighbors, fold.vec, n.folds)
  if(!((length(TrainingInput[,1]) == length(TrainingLabel)) && ( length(TrainingLabel) == length(fold.vec))))
  {
    print(sprintf("length(TrainingInput):%d length(TrainingLabel):%d length(fold.vec):%d",length(TrainingInput),length(TrainingLabel),length(fold.vec)))
    stop("Error: (All 3 Rows(TrainingInput(Parameter(0)),TrainingLabel( Parameter(1)),fold.vec( Parameter(3))) must be equal length")
  }
  
  
  if(!(as.character(typeof(TrainingInput)) %in% c("list","integer","double")))
  {
    print(sprintf("typeof(TrainingInput): %s, and is not a; list,integer, or double"),typeof(TrainingInput))
    stop("Error: (Parameter(0)) 'TrainingData' contains an element not a list,integer, or double")
  }
  if(!(as.character(typeof(TrainingLabel)) %in% c("list","integer","double")))
  {
    print(sprintf("typeof(TrainingLabel): %s, and is not a; list,integer, or double"),typeof(TrainingLabel))
    stop("Error: (Parameter(1)) 'TrainingLabel' contains an element not a list,integer, or double")
  }
  
  print("Passed Sanitation Tests, calling KNNLearnCV.Algorithm")
  KNNLearnCV.Algorithm(TrainingInput, TrainingLabel, max.neighbors, fold.vec, n.folds)
}
