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


#Use NNLearnCV to train a model on the other two folds (which should be used in
#   your NNLearnCV function as internal train/validation sets/splits), then make
#   a prediction on the test fold s.

#X.mat, y.vec is a training data set.
#fold.vec is a vector of fold ID numbers. If fold.vec is NULL, randomly assign
#   fold IDs from 1 to n.folds, i.e.
#-fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
#TODO
# -Fix Stop Errors to acctually detect missing or Spoofed Values
NNLearnCV<-function(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  #the Fold.Vect is the Fold value considered the Testing DataSet,
  #Everything else is appart of the Training DataSet(For this instance/'Function call')
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
  if(fold.vec == NULL){
    print("Warning: ( Parameter(3)) 'fold.vec' contained no value \n
         Should Contain a vector of fold ID numbers")
    fold.vec <-Random_Folds(length(Y.vec),5)
  }

  Error.mat = list()
  
  # Use this as an example to loop over Folds
  # L1-Norm Loss function
  for(fold.i in seq_along(unique.folds)){
    for(prediction.set.name in c("train", "validation")){
      
      pred.mat <- NN1toKmaxPredict(
        train.features, train.labels,
        prediction.set.features, max.neighbors)
      
      loss.mat <- if(labels.all.01)
      {
        ifelse(pred.mat>0.5, 1, 0) != y.vec #zero-one loss for binary classification.
      }
      else
      {
        abs(pred.mat-y.vec) #L1-Norm Loss function
      }
      Error.mat[, fold.i] <- colMeans(loss.mat)
    }
  }


  for (Iteration in 0:Folds)
  {
    Find_OptimalKNN()
    #Use L1/Manhattan as the learning loss
  }

  #return a list with the following named elements:
  #  X.mat, y.vec: training data.

  #train.loss.mat, validation.loss.mat (matrices of loss values for each fold and number of neighbors).
  #train.loss.vec, validation.loss.vec (vectors with max.neighbors elements: mean loss over all folds).
  #selected.neighbors (number of neighbors selected by minimizing the mean validation loss).
  #predict(testX.mat), a function that takes a matrix of inputs/features and returns a vector of predictions. It should check the type/dimension of testX.mat and stop() with an informative error message if there are any issues.
  0
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


