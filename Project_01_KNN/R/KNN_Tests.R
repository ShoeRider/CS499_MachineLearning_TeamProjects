library(rbenchmark)
library(Rcpp)

#include local files
sourceCpp("../src/KNN.cpp")


set.seed(123)
z <- rnorm(100000)
x <- rnorm(100)

# check that SortVector_Eigen is the same as sort
stopifnot(all.equal(SortVector_Eigen(x), sort(x)))


# benchmark SortVector_Eigen and sort
benchmark(SortVector_Eigen(z), sort(z), order="relative")[,1:4]

knn_R <- function(TrainingInput, TrainingLabel, Neighbors, TestInput, TestPrediction)
{
  sample(1:6,10,replace=T)
}

#This function works better with larger lists!!
#note RandomNumbers generate (0,Folds)
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Need A Larger Array Size!!"))
  try(if(Folds < 0) stop("Need A Larger Folds Value!!"))
  sample(0:(Folds-1),Size,replace=T)
}



#Use NNLearnCV to train a model on the other two folds (which should be used in
#   your NNLearnCV function as internal train/validation sets/splits), then make
#   a prediction on the test fold s.

#X.mat, y.vec is a training data set.
#fold.vec is a vector of fold ID numbers. If fold.vec is NULL, randomly assign 
#   fold IDs from 1 to n.folds, i.e.
#-fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
NNLearnCV<-function(X.mat, Y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  if(length(X.mat) == 0){
    fold.vec = 1
  }
  if(length(Y.vec) == 0){
    fold.vec = 1
  }
  if(length(fold.vec) == 0){
    fold.vec = 1
  }
  fold.vec
  
  
  
  0
}







#Testing Datasets:
#Binary classification.
ElemStatLearn::spam
#ElemStatLearn::spam is a 2-class [4601, 57] output is last column (spam).



#ElemStatLearn::SAheart 2-class [462, 9] output is last column (chd).
#ElemStatLearn::zip.train: 10-class [7291, 256] output is first column. (ignore classes other than 0 and 1)
#Regression.
#ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
#ElemStatLearn::ozone [111 x 3] output is first column (ozone)



CPP_Vs_R_Compairison <-function()
{

}


#couldnt get 'prim' to work.... 
install.packages("PRIMsrc")
library(PRIMsrc)

install.packages("PRIMsrc")
require(PRIMsrc)
install.packages("prim")
library(prim)
require("prim")





#Binary Tests
KNN_Spam_Test<-function()
{
  Folds <- 3
  MaxNeighbors <- 30
  Local_spam<- ElemStatLearn::spam

  DataCols = length(Local_spam) - 1
  LabelCol = length(Local_spam)
  Rows = length(Local_spam[,1])

  #accesses first two Rows:
  #ElemStatLearn::spam[1:2,]

  #Create New Fold Column to hold Fold Values
  Local_spam$Fold <- Random_Folds(length(Local_spam[,1]),Folds)

  #implement NNLearnCV
  NNLearnCV(Local_spam[,0:DataCols], Local_spam[,LabelCol], MaxNeighbors, Local_spam$Fold, Folds)
  
  #plot the mean validation loss as a function of the number of neighbors.
  #plot the mean train loss in one color, and the mean validation loss in another color. 
}



KNN_SAheart_Test<-function()
{
  Local_SAheart<- ElemStatLearn::SAheart
  
}

KK_ziptrain_Test<-function()
{
  #output is first column, 
  #  and ignore classes other than 0 and 1
  Local_ZipTrain<- ElemStatLearn::zip.train
  
}




#Regression Tests
