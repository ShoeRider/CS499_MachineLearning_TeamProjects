# to use these tests include these commands:
if (!require("ElemStatLearn")) install.packages("ElemStatLearn")
if (!require("testthat")) install.packages("testthat")
library(testthat)
library(ElemStatLearn)
#library(NearestNeighbors)
context("knn")



source("R/knn.R")








#This function works better with larger lists!!
#note RandomNumbers generate (0,Folds)
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}



#Binary Tests
KNN_Spam_Test<-function()
{
  print("Starting Spam_Test")
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
  Projected_K = KNNLearnCV(Local_spam[,DataColsStart:DataColsEnd], Local_spam[,LabelCol], MaxNeighbors, Local_spam$Fold, Folds)


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
KNN_Spam_Test()




#ElemStatLearn::SAheart 2-class [462, 9] output is last column (chd).
KNN_SAheart_Test<-function()
{
  print("Starting Local_SAheart")
  Folds <- 3
  MaxNeighbors <- 30
  Local_SAheart<- ElemStatLearn::SAheart


  DataColsStart = 0
  DataColsEnd   = length(Local_SAheart) - 1
  LabelCol      = length(Local_SAheart)
  Rows          = length(Local_SAheart[,1])

  #accesses first two Rows:
  #Local_SAheart[1:2,]
  famhist<-factor(c("Present" = 1, "Absent" = 0))

  #Local_SAheart[,5] <- as.integer(factor(Local_SAheart[,5],levels=c("Present" = 1, "Absent" = 2)))
  Local_SAheart[,5] <- sapply(as.character(Local_SAheart[,5]),switch,"Present"=1,"Absent"=2)
  print(Local_SAheart)
  #Local_SAheart <-factor(Local_SAheart[,5])

  #Create New Fold Column to hold Fold Values
  Local_SAheart$Fold <- Random_Folds(length(Local_SAheart[,1]),Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Projected_K = KNNLearnCV(Local_SAheart[,DataColsStart:DataColsEnd], Local_SAheart[,LabelCol], MaxNeighbors, Local_SAheart$Fold, Folds)
}
#KNN_SAheart_Test()



#ElemStatLearn::zip.train: 10-class [7291, 256] output is first column. (ignore classes other than 0 and 1)
KNN_ziptrain_Test<-function()
{
  #output is first column,
  #  and ignore classes other than 0 and 1

  print("Starting Local_ZipTrain")
  Folds <- 3
  MaxNeighbors <- 30
  Local_ZipTrain<- ElemStatLearn::zip.train

  for (Filter in c(0,1)){
    NonZero <- which(Local_ZipTrain[,1]!=Filter,arr.ind=TRUE)
    #print(testIndexes)
    Local_ZipTrain  <- Local_ZipTrain[NonZero, ]
  }

  DataColsStart = 2
  DataColsEnd   = length(Local_ZipTrain[1,])
  LabelCol      = 1
  Rows          = length(Local_ZipTrain[,1])

  #accesses first two Rows:
  #Local_SAheart[1:2,]
  #print(Local_ZipTrain[,1])


  print(Local_ZipTrain[Rows,])
  print(Rows)


  #Create New Fold Column to hold Fold Values
  Fold.vec <- Random_Folds(Rows,Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Projected_K = KNNLearnCV(Local_ZipTrain[,DataColsStart:DataColsEnd], Local_ZipTrain[,LabelCol], MaxNeighbors, Fold.vec, Folds)

}
#KNN_ziptrain_Test()



#Regression.
#ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
KNN_prostate<-function()
{

  print("Starting KNN_prostate")
  Folds <- 3
  MaxNeighbors <- 30
  Local_prostate<- ElemStatLearn::prostate[,1:9]


  DataColsStart = 2
  DataColsEnd   = length(Local_prostate[1,]) -1
  LabelCol      = length(Local_prostate[1,])
  Rows          = length(Local_prostate[,1])

  #accesses first two Rows:
  #Local_SAheart[1:2,]
  #print(Local_prostate[,1])


  print(Local_prostate[Rows,])
  print(Rows)


  #Create New Fold Column to hold Fold Values
  Fold.vec <- Random_Folds(Rows,Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Projected_K = KNNLearnCV(Local_prostate[,DataColsStart:DataColsEnd], Local_prostate[,LabelCol], MaxNeighbors, Fold.vec, Folds)
}
#KNN_prostate()

#ElemStatLearn::ozone [111 x 3] output is first column (ozone)
KNN_ozone<-function()
{
  #output is first column,
  #  and ignore classes other than 0 and 1

  print("Starting Local_Ozone")
  Folds <- 3
  MaxNeighbors <- 30
  Local_ZipTrain<- ElemStatLearn::prostate


  DataColsStart = 2
  DataColsEnd   = length(Local_ZipTrain[1,])
  LabelCol      = 1
  Rows          = length(Local_ZipTrain[,1])

  #accesses first two Rows:
  #Local_SAheart[1:2,]
  #print(Local_ZipTrain[,1])


  print(Local_ZipTrain[Rows,])
  print(Rows)


  #Create New Fold Column to hold Fold Values
  Fold.vec <- Random_Folds(Rows,Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Projected_K = KNNLearnCV(Local_ZipTrain[,DataColsStart:DataColsEnd], Local_ZipTrain[,LabelCol], MaxNeighbors, Fold.vec, Folds)

}
#KNN_ozone()
