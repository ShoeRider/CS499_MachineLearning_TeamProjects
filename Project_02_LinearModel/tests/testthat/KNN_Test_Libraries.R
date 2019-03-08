# to use these tests include these commands:
if (!require("ElemStatLearn")) install.packages("ElemStatLearn")
if (!require("testthat")) install.packages("testthat")

library(ElemStatLearn)
library(testthat)

library(LinearModel)
context("LinearModel")


source("tests/testthat/Prep_Libraries.R")





#Binary Tests
KNN_Spam_Test<-function()
{
  Spam<-Prep_Spam()
  #print(Spam$TrainingData)
  KNNLearnCV(Spam$TrainingData, Spam$TrainingLabels, Spam$MaxNeighbors, Spam$Folds.Vec, Spam$Folds.n)

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
  Local_SAheart[,5] <- sapply(as.character(Local_SAheart[,5]),switch,"Present"=1,"Absent"=0)
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
  DataColsEnd   = NCOL(Local_ZipTrain)
  LabelCol      = 1
  Rows          = NROW(Local_ZipTrain)

  #accesses first two Rows:
  #Local_SAheart[1:2,]
  #print(Local_ZipTrain[,1])


  print(DataColsEnd)
  print(Rows)
  #print(Local_ZipTrain)

  #Create New Fold Column to hold Fold Values
  Fold.vec <- Random_Folds(Rows,Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Projected_K = KNNLearnCV(Local_ZipTrain[,DataColsStart:DataColsEnd], Local_ZipTrain[,LabelCol], MaxNeighbors, Fold.vec, Folds)

  print(((Projected_K)))
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
  Local_ozone<- ElemStatLearn::ozone


  DataColsStart = 2
  DataColsEnd   = NCOL(Local_ozone)
  LabelCol      = 1
  Rows          = NROW(Local_ozone)

  #accesses first two Rows:
  #Local_SAheart[1:2,]
  #print(Local_ZipTrain[,1])


  print(Local_ozone)
  print(Rows)


  #Create New Fold Column to hold Fold Values
  Fold.vec <- Random_Folds(Rows,Folds)


  #Double Folding ? ~still a little confused where to implement the two instances of the Folds
  Projected_K = KNNLearnCV(Local_ozone[,DataColsStart:DataColsEnd], Local_ozone[,LabelCol], MaxNeighbors, Fold.vec, Folds)

}
#KNN_ozone()
