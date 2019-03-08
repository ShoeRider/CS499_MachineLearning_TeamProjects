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
                    predictions=double(max.neighbors),PACKAGE="NearestNeighbors")
}


#library(NearestNeighbors)
#library(testthat)
#context("knn")

Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}


KNNLearnCV.Create.Fold.Vec<- function(Array,Folds)
{
  try(if(ArraySize <= 0) stop("Invalid ArraySize Value: cannot preform random Folds when (ArraySize < 0) !!"))
  try(if(Folds <= 0) stop("Invalid Folds Value: cannot preform random Folds when (Folds < 0) !!"))
  print(ArraySize)
  Fold.Vec <- cut(seq(1,nrow(Array),breaks=3,labels=FALSE))
}


KNNLearnCV.Algorithm<-function(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  Data = cbind(X.mat ,Y.vec)
  NonZero= 0
  BinaryClassification =  all(Y.vec <= 1 & Y.vec >= 0)

  print(BinaryClassification)

  DataColsStart = 0
  DataColsEnd   = NCOL(Data) - 1
  LabelCol      = NCOL(Data)
  Rows          = NROW(Data)

  #print(Rows)



  Training.L1Error.matrix = 0
  #Training.L2Error.matrix = 0

  Testing.L1Error.matrix = 0
  Testing.L2Error.matrix = 0

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

    if(typeof(train.Data) =="double")
    {
      Double.train.Data   <- train.Data
    }else{
      Double.train.Data   <- do.call(cbind, lapply(train.Data, as.numeric))
    }

    if(typeof(train.Labels) =="double")
    {
      Double.train.Labels   <- train.Labels
    }else{
      Double.train.Labels   <- do.call(cbind, lapply(train.Labels, as.numeric))
    }

    if(typeof(test.Data) =="double")
    {
      Double.test.Data   <- test.Data
    }else{
      Double.test.Data    <- do.call(cbind, lapply(test.Data, as.numeric))
    }




    #print(test.Labels)
    #print(length(test.Labels[test.Labels == 1]))
    #print(length(test.Labels[test.Labels == 2]))

    #print(t(Double.train.Labels))
    #print(length(Double.train.Labels[t(Double.train.Labels) == 1]))
    #print(length(Double.train.Labels[t(Double.train.Labels) == 2]))


    for(X in 1:NROW(Double.test.Data))
    {
      Error.vec <- knn(Double.train.Data, Double.train.Labels, Double.test.Data[X,], max.neighbors)[7]

      if(BinaryClassification)
      {
        L1Error.vec <- ifelse(do.call(cbind, lapply(Error.vec, as.numeric))>0.5,1,0) != as.integer( t(test.Labels)[X])
      }else{
        L1Error.vec <- abs(do.call(cbind, lapply(Error.vec, as.numeric)) - as.integer( t(test.Labels)[X]))
      }
      Testing.L1Error.matrix <-rbind(Testing.L1Error.matrix,t(L1Error.vec ))


      if(BinaryClassification)
      {
        L2Error.vec <- ifelse(do.call(cbind, lapply(Error.vec, as.numeric))>0.5,1,0) != as.integer( t(test.Labels)[X])
      }else{
        L2Error.vec <- (do.call(cbind, lapply(Error.vec, as.numeric)) - as.integer( t(test.Labels)[X]))**2
      }


      Testing.L2Error.matrix <-rbind(Testing.L2Error.matrix,t(L2Error.vec ))



    }

    #for(X in 1:NROW(Double.train.Data))
    #{
    #  Error.vec <- knn(Double.train.Data, Double.train.Labels, Double.train.Data[X,], max.neighbors)[7]

    #  L1Error.vec <- abs(do.call(cbind, lapply(Error.vec, as.numeric)) - as.integer( t(Double.train.Labels)[X]))
    #  Training.L1Error.matrix <-rbind(Training.L1Error.matrix,t(L1Error.vec ))

    #}

    #Prediction <- knn(Double.train.Data, Double.train.Labels, Double.test.row, max.neighbors)  - (test.Labels)

    #Error.Vector = abs(Prediction - (test.Labels))
    #print((knn(train.Data, train.Labels, test.Data, max.neighbors)))
    #print(length(test.Labels))
    #print(Error.matrix)
    #Error.matrix <- rbind(Error.matrix,Error.Vector)
  }
  print(colMeans(Testing.L1Error.matrix))
  print(colMeans(Testing.L2Error.matrix))
  #print(colMeans(Training.L1Error.matrix))

  #plot(colMeans(Testing.L2Error.matrix),type="o", col = "blue", xlab = "K Neighbors", ylab = "Error",main = "KNN wrt Selected K, ElemStatLearn::Spam\n L1(red),L2(blue)")
  #lines(colMeans(Testing.L1Error.matrix), type = "o", col = "red")

  if(BinaryClassification)
  {
    plot(colMeans(Testing.L1Error.matrix),type="o", col = "Dark Green", xlab = "K Neighbors", ylab = "Error",main = "KNN wrt Selected K, ElemStatLearn::SAheart\n if n>.5,then 1, otherwise 0")
  }
  else{
    plot(colMeans(Testing.L1Error.matrix),type="o", col = "red", xlab = "K Neighbors", ylab = "Error",main = "KNN wrt Selected K, ElemStatLearn::Spam\n L1(red),L2(blue)")
    lines(colMeans(Testing.L2Error.matrix), type = "o", col = "blue")

    #plot(colMeans(Testing.L2Error.matrix),type="o", col = "blue", xlab = "K Neighbors", ylab = "Error",main = "KNN wrt Selected K, ElemStatLearn::Spam\n L1(red),L2(blue)")
    #lines(colMeans(Testing.L1Error.matrix), type = "o", col = "red")
  }


  #plot(colMeans(Training.L1Error.matrix),type="o", col = "blue", xlab = "K Neighbors", ylab = "Error",main = "KNN wrt Selected K, ElemStatLearn::Ozone\n Testing(red),Training(blue)")
  #lines(colMeans(Testing.L1Error.matrix), type = "o", col = "red")


  #plot(colMeans(Testing.L1Error.matrix),type="o", col = "red", xlab = "K Neighbors", ylab = "Error",main = "KNN wrt Selected K, ElemStatLearn::Ozone\n Testing(red),Training(blue)")
  #lines(colMeans(Training.L1Error.matrix), type = "o", col = "blue")


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

  print("Passed Sanitation Tests, calling KNNLearnCV.Algorithm")
  knnLearncv <- KNNLearnCV.Algorithm(TrainingInput, TrainingLabel, max.neighbors, fold.vec, n.folds)
}

