library(nearestNeighborsAlg)
library(testthat)
context("knn")

test_that("knn computes same answer as R", {
  data(zip.train, packages= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]
  testx <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- knn(x,y,testx,max.neighbors)
  dist.mat <- t(x) - testx
  dist.vec <- sqrt( colSums(dist.mat * dist.mat))
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max.neighbors]
  expected.prediction <- cumsum(y[closest.indices])/(1:max.neighbors)
  expect_equal(pred.vec,expected.prediction)

})


# to use these tests include these commands:
install.packages("ElemStatLearn")
library(ElemStatLearn)



#This function works better with larger lists!!
#note RandomNumbers generate (0,Folds)
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}

sanitize_TrainingData<-function(Training.Input, Training.Label)
{
  if(length(TrainingInput) == length(TrainingLabel))
  {
    print(length(TrainingInput))
    print(length(TrainingLabel))
    stop("( The Rows for Both (Parameter(0)) Training.Input, and (Parameter(0)) Training.Label, must be the same Length")
  }
  
  if(length(X.mat) == 0){
    stop("( Parameter(0)) 'Training Data' contains a length of 0, or no elements \n
         Should Contain a Matrix")
  }
  if(length(Y.vec) == 0){
    stop("( Parameter(1)) 'Training Labels' contains a length of 0, or no elements \n
         Should Contain a list of Tablels")
  }
  if(n.folds == 0){
    stop("( Parameter(4)) 'n.folds' contains novalue, \n
         and should contain the number of Folds to preform")
  }
  if(length(fold.vec) == 0){
    print("Warning: ( Parameter(3)) 'fold.vec' contained no value \n
          Should Contain a vector of fold ID numbers")
    fold.vec <-Random_Folds(length(Y.vec),5)
  }
  if(typeof(Training.Input) != "double")
  {
    print(typeof(Training.Input))
    stop("(Parameter(0)) 'Training Data' contains an element not a double")
  }
  if(typeof(Training.Label) != "double")
  {
    print(typeof(Training.Label))
    stop("(Parameter(0)) 'Training.Label' contains an element not a double")
  }
  if(length(TrainingInput) == 0){
    stop("( Parameter(0)) 'Training Data' contains a length of 0, or no elements \n
         Should Contain a Matrix")
  }
  if(length(TrainingLabel) == 0){
    stop("( Parameter(1)) 'Training Labels' contains a length of 0, or no elements \n
         Should Contain a list of Tablels")
  }
}
      
NNLearnCV<-function(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  #the Fold.Vect is the Fold value considered the Testing DataSet,
  #Everything else is appart of the Training DataSet(For this instance/'Function call')
  #sanitize_TrainingData(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
  
  Data = cbind(X.mat ,Y.vec)

  DataColsStart = 0
  DataColsEnd   = length(Data) - 1
  LabelCol      = length(Data)
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



      
      
   
#Binary Tests
KNN_Spam_Test<-function()
{
  Folds <- 3
  MaxNeighbors <- 30
  Local_spam<- ElemStatLearn::spam


  DataColsStart = 0
  DataColsEnd   = length(Local_spam) - 1
  LabelCol      = length(Local_spam)
  Rows          = length(Local_spam[,1])

  #accesses first two Rows:
  #Local_spam[1:2,]

  #Create New Fold Column to hold Fold Values
  Local_spam$Fold <- Random_Folds(length(Local_spam[,1]),Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  for (Iteration in 0:Folds)
  {
    print(Iteration)
    #implement NNLearnCV
    Projected_K = NNLearnCV(Local_spam[,DataColsStart:DataColsEnd], Local_spam[,LabelCol], MaxNeighbors, Local_spam$Fold, Folds)

  }

  #

  #For each train/test split, to show that your algorithm is actually learning something non-trivial
  #from the inputs/features, compute a baseline predictor that ignores the inputs/features.?
  #  Not exactly sure what is asked, I believe he wants the : (for an arbitrary K value)
    #plot the mean validation loss as a function of the number of neighbors.
    #plot the mean train loss in one color, and the mean validation loss in another color.

  #For each data set, compute a 2 x 3 matrix of mean test loss values:
    #each of the three columns are for a specific test set,
    #the first row is for the nearest neighbors predictor,
    #the second row is for the baseline/un-informed predictor.

  #plot the mean validation loss as a function of the number of neighbors.
  #plot the mean train loss in one color, and the mean validation loss in another color.
}



#ElemStatLearn::SAheart 2-class [462, 9] output is last column (chd).
KNN_SAheart_Test<-function()
{
  Local_SAheart<- ElemStatLearn::SAheart

}

#ElemStatLearn::zip.train: 10-class [7291, 256] output is first column. (ignore classes other than 0 and 1)
NKK_ziptrain_Test<-function()
{
  #output is first column,
  #  and ignore classes other than 0 and 1
  Local_ZipTrain<- ElemStatLearn::zip.train

}




#Regression.
#ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
#ElemStatLearn::ozone [111 x 3] output is first column (ozone)


