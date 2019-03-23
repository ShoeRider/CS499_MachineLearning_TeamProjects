#knn_interface
#source("R/NNLearnCV.R")




# @param hyperparameter k, The Number of Nearest neighbors compaired to when predicting a value.

#' KNNLearnCV, This function validates the data, and verifies that the parameters are conformed to each other, and that they are indeed 
#' formated correctly It calls KNNLearnCV.Algorithm from the knn.R file, and returns the list generated. It contains the same Parameters, and 
#' functionality as KNNLearnCV.Algorithm, here is a full discription of the function:
#' Simple funtion that calls NN1toKmaxPredict.R function, and finds the most opatimal K value for KNN.
#' Returns a list of the produced Error Mean, a function wich uses the found hyperparameter K, with Lowest Error.
#'  
#'
#'@param TrainingData numeric imput feature matrix [n x p],
#'@param TrainingLabels numberic input label vector [n], 
#'either all 0/1 for binary classification or other real numbers for regression
#'@param TestData numberic test festure vector [p], that is used to test the different hyperparameters for k (1 to MAX.K) of KNN
#'@param max.neighbors scalar integer, max number of neighbors
#'@param fold.vec vector indicating the fold each element is in [n]
#'@param fold.n the number of folds that are to be preformed
#'
#'@return Returns a list with the following elements 
#'      X.mat              = TrainingData numeric imput feature matrix [n x p],
#'      y.vec              = TrainingLabels numberic input label vector [n], 
#'      train.loss.mat     = 
#'      train.loss.vec     = Mean Error for each 1toKmax solution,
#'      selected.neighbors = The integer value of the otimal K for KNN,
#'      Predict            = Predict(X.), Function to preform the optimal K nearest neighbors selected,
#'@export
#'
#'@examples
#'~~ Example 1 ~~
#'Spam<-Prep_Spam()
#'Fold.vec = Random_Folds(Spam$n_Elements,4)
#'Fold.n   = 4
#'KNNLearnCV.List = NN1toKmaxPredict(TrainingData, TrainingLabels,TestData, 30)
#' #Where KNNLearnCV.List is a list containing the elements above, and you are free to use the returned values as you wish.
#' 
#'barplot(KNNLearnCV.List$train.loss.vec,main = "Spam: KNNLearnCV.L2TestError.FoldMeans",xlab = "KNN Compared",ylab = "Error",beside = TRUE)
#'
#'~~ Example 2 ~~
#'
#'Fold.vec = Random_Folds($n_Elements,4)
#'Fold.n   = 4
#'KNNLearnCV.List = NN1toKmaxPredict(TrainingData, TrainingLabels,TestData, 30)
#' #Where KNNLearnCV.List is a list containing the elements above, and you are free to use the returned values as you wish.
#' 
#'barplot(KNNLearnCV.List$train.loss.vec,main = "Spam: KNNLearnCV.L2TestError.FoldMeans",xlab = "KNN Compared",ylab = "Error",beside = TRUE)
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
    fold.vec <-KNNLearnCV.Create.Fold.Vec(length(TrainingLabel),n.folds)
  }
  if(!(length(dim(fold.vec)) %in% c(0,1)))
  {
    print("Warning: ( Parameter(3)) (length(dim(fold.vec)) is not 0 or 1!!")
    print("a new set fold.vec will be created to produce a result")
    fold.vec <-KNNLearnCV.Create.Fold.Vec(length(TrainingInput[,1]),n.folds)
  }
  if(!(as.character(typeof(fold.vec)) %in% c("list","integer","double")))
  {
    print(sprintf("Warning: ( Parameter(3)) 'fold.vec' contains an element not a list, integer, or double, but as a typeof: %s ",typeof(fold.vec)))
    print(sprintf("Try Passing Parameter(3) like so (..,list(Parameter(3)),..) 'fold.vec' contains an element not a list or double "))
    print("a new set fold.vec will be created to produce a result")
    fold.vec <-KNNLearnCV.Create.Fold.Vec(Training.Instances,n.folds)
  }
  if((length(fold.vec) == 0))
  {
    print("Warning: ( Parameter(3)) 'fold.vec'  contains a length of 0, or no elements")
    print("a new set fold.vec will be created to produce a result")
    fold.vec <-KNNLearnCV.Create.Fold.Vec(Training.Instances,n.folds)
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
    print(as.character(typeof(TrainingInput)))
    if(as.character(typeof(TrainingInput)) == "list")
    {
      TrainingInput <- as.numeric(unlist(TrainingInput))
    }
    else
    {
      print(as.character(typeof(TrainingInput)))
      print(sprintf("typeof(TrainingInput): %s, and is not a; list,integer, or double"),as.character(typeof(TrainingInput)))
      stop("Error: (Parameter(0)) 'TrainingData' contains an element not a list,integer, or double")
    }
  }
  if(!(as.character(typeof(TrainingLabel)) %in% c("integer","double")))
  {
    if(as.character(typeof(TrainingLabel)) == "list")
    {
      TrainingLabel <- as.numeric(unlist(TrainingLabel))
    }
    else
    {
      print(as.character(typeof(TrainingLabel)))
      print(sprintf("typeof(TrainingLabel): %s, and is not a; list,integer, or double"),as.character(typeof(TrainingLabel)))
      stop("Error: (Parameter(0)) 'TrainingLabel' contains an element not a list,integer, or double")
    }
    
  }
  
  #print("Passed Sanitation Tests, calling KNNLearnCV.Algorithm")
  return(KNNLearnCV.Algorithm(TrainingInput, TrainingLabel, max.neighbors, fold.vec, n.folds))
}

