# to use these tests include these commands:
if (!require("ElemStatLearn")) install.packages("ElemStatLearn")
if (!require("testthat")) install.packages("testthat")
#library(testthat)
library(ElemStatLearn)

context("LinearModel")



#source("R/knn.R")








#This function works better with larger lists!!
#note RandomNumbers generate (0,Folds)
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}



#Binary Tests



Linear_Spam_Tests<-function()
{
  print("Starting Spam_Test")
  Folds <- 3
  MaxNeighbors <- 30
  Local_spam<- ElemStatLearn::spam

  BinaryClassification = 1
  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])


  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])
  print(NROW(Spam))


  email = Local_spam[0,]
  email <- head(Local_spam[Local_spam$spam == 0,],MaxSample_ofType)
  #print(NROW(email))

  Cliped<-rbind(Spam,email)
  Cliped<-Cliped[sample(nrow(Cliped)),]
  DataColsStart = 0
  DataColsEnd   = NCOL(Cliped) - 1
  LabelCol      = NCOL(Cliped)
  Rows          = NROW(Cliped)

  #Create New Fold Column to hold Fold Values
  fold.vec <- Random_Folds(Rows,Folds)

  TrainingLabels <- data.matrix(Cliped[,LabelCol])
  TrainingData   <- data.matrix(Cliped[,DataColsStart:DataColsEnd])

  #Question:1
  #DeNormalizedWeights <- LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],30,0.1)



  #Question:2
  #DeNormalizedWeights<-LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol], fold.vec,Folds,30)
  #ES.List <-LMSquareLossEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec,Folds,30)
  #print(ES.List)
  #DeNormalizedWeights <- ES.List$w.mat


  #Questions: (3)
  #Penalty.Vector <-array(rep(0.01,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))


  #Initial.W.Vector<-array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))

  #print("Initial weight vector")
  #print(dim(Initial.W.Vector))

  #TrainingData.mean <- mean(TrainingData)
  #TrainingData.Sum = 0

  #for( row in 1:nrow(TrainingData))
  #{
  #  TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  #}

  #get sd from the calculated sum and number of observations
  #TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))
  #Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #Question: 3 Function call
  #Penalty.Scalar=2
  #W.Matrix <-LMSquareLossL2(Normalized_TrainingData,  TrainingLabels, Penalty.Scalar, .31,Initial.W.Vector)
  #LMSquareLossL2(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)

  #print(dim(W.Matrix))

  #DeNormalizedWeights<-(t(W.Matrix))/TrainingData.sd

  #print(ES.List$)

  #print("DIM of DeNormalizedWeights")
  #print(dim(DeNormalizedWeights))
  #print(DeNormalizedWeights)

  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],DeNormalizedWeights,BinaryClassification)
  #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],t(DeNormalizedWeights),BinaryClassification)
  #barplot(DeNorm.Error,main = "L1 Error :Question 3 spam",xlab = "mean loss value",beside = TRUE)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)



  #Question: 4
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2penalties(TrainingData, TrainingLabels,Penalty.Vector)

  DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],W.Matrix,BinaryClassification)
  print(DeNorm.Error)
  barplot(DeNorm.Error,main = "Q4 penalty.vector Error: spam",xlab = "mean loss value",beside = TRUE)



  #Question:5
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2CV(TrainingData, TrainingLabels,Penalty.Vector)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)
}
Linear_Spam_Tests()






#ElemStatLearn::SAheart 2-class [462, 9] output is last column (chd).
Linear_SAheart_Test<-function()
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


  #Question:1
  #DeNormalizedWeights <- LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],30,0.1)



  #Question:2
  #DeNormalizedWeights<-LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol], fold.vec,Folds,30)
  #ES.List <-LMSquareLossEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec,Folds,30)
  #print(ES.List)
  #DeNormalizedWeights <- ES.List$w.mat


  #Questions: (3)
  #Penalty.Vector <-array(rep(0.01,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))


  #Initial.W.Vector<-array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))

  #print("Initial weight vector")
  #print(dim(Initial.W.Vector))

  #TrainingData.mean <- mean(TrainingData)
  #TrainingData.Sum = 0

  #for( row in 1:nrow(TrainingData))
  #{
  #  TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  #}

  #get sd from the calculated sum and number of observations
  #TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))
  #Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #Question: 3 Function call
  #Penalty.Scalar=2
  #W.Matrix <-LMSquareLossL2(Normalized_TrainingData,  TrainingLabels, Penalty.Scalar, .31,Initial.W.Vector)
  #LMSquareLossL2(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)

  #print(dim(W.Matrix))

  #DeNormalizedWeights<-(t(W.Matrix))/TrainingData.sd

  #print(ES.List$)

  #print("DIM of DeNormalizedWeights")
  #print(dim(DeNormalizedWeights))
  #print(DeNormalizedWeights)

  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],DeNormalizedWeights,BinaryClassification)
  #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],t(DeNormalizedWeights),BinaryClassification)
  #barplot(DeNorm.Error,main = "L1 Error :Question 3 spam",xlab = "mean loss value",beside = TRUE)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)



  #Question: 4
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2penalties(TrainingData, TrainingLabels,Penalty.Vector)

  DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],W.Matrix,BinaryClassification)
  print(DeNorm.Error)
  barplot(DeNorm.Error,main = "Q4 penalty.vector Error: spam",xlab = "mean loss value",beside = TRUE)



  #Question:5
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2CV(TrainingData, TrainingLabels,Penalty.Vector)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)
}
#Linear_SAheart_Test()



#ElemStatLearn::zip.train: 10-class [7291, 256] output is first column. (ignore classes other than 0 and 1)
Linear_ziptrain_Test<-function()
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


  #Question:1
  #DeNormalizedWeights <- LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],30,0.1)



  #Question:2
  #DeNormalizedWeights<-LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol], fold.vec,Folds,30)
  #ES.List <-LMSquareLossEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec,Folds,30)
  #print(ES.List)
  #DeNormalizedWeights <- ES.List$w.mat


  #Questions: (3)
  #Penalty.Vector <-array(rep(0.01,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))


  #Initial.W.Vector<-array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))

  #print("Initial weight vector")
  #print(dim(Initial.W.Vector))

  #TrainingData.mean <- mean(TrainingData)
  #TrainingData.Sum = 0

  #for( row in 1:nrow(TrainingData))
  #{
  #  TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  #}

  #get sd from the calculated sum and number of observations
  #TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))
  #Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #Question: 3 Function call
  #Penalty.Scalar=2
  #W.Matrix <-LMSquareLossL2(Normalized_TrainingData,  TrainingLabels, Penalty.Scalar, .31,Initial.W.Vector)
  #LMSquareLossL2(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)

  #print(dim(W.Matrix))

  #DeNormalizedWeights<-(t(W.Matrix))/TrainingData.sd

  #print(ES.List$)

  #print("DIM of DeNormalizedWeights")
  #print(dim(DeNormalizedWeights))
  #print(DeNormalizedWeights)

  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],DeNormalizedWeights,BinaryClassification)
  #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],t(DeNormalizedWeights),BinaryClassification)
  #barplot(DeNorm.Error,main = "L1 Error :Question 3 spam",xlab = "mean loss value",beside = TRUE)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)



  #Question: 4
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2penalties(TrainingData, TrainingLabels,Penalty.Vector)

  DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],W.Matrix,BinaryClassification)
  print(DeNorm.Error)
  barplot(DeNorm.Error,main = "Q4 penalty.vector Error: spam",xlab = "mean loss value",beside = TRUE)



  #Question:5
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2CV(TrainingData, TrainingLabels,Penalty.Vector)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)
}
#Linear_ziptrain_Test()



#Regression.
#ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
Linear_prostate<-function()
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


  #Question:1
  #DeNormalizedWeights <- LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],30,0.1)



  #Question:2
  #DeNormalizedWeights<-LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol], fold.vec,Folds,30)
  #ES.List <-LMSquareLossEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec,Folds,30)
  #print(ES.List)
  #DeNormalizedWeights <- ES.List$w.mat


  #Questions: (3)
  #Penalty.Vector <-array(rep(0.01,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))


  #Initial.W.Vector<-array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))

  #print("Initial weight vector")
  #print(dim(Initial.W.Vector))

  #TrainingData.mean <- mean(TrainingData)
  #TrainingData.Sum = 0

  #for( row in 1:nrow(TrainingData))
  #{
  #  TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  #}

  #get sd from the calculated sum and number of observations
  #TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))
  #Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #Question: 3 Function call
  #Penalty.Scalar=2
  #W.Matrix <-LMSquareLossL2(Normalized_TrainingData,  TrainingLabels, Penalty.Scalar, .31,Initial.W.Vector)
  #LMSquareLossL2(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)

  #print(dim(W.Matrix))

  #DeNormalizedWeights<-(t(W.Matrix))/TrainingData.sd

  #print(ES.List$)

  #print("DIM of DeNormalizedWeights")
  #print(dim(DeNormalizedWeights))
  #print(DeNormalizedWeights)

  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],DeNormalizedWeights,BinaryClassification)
  #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],t(DeNormalizedWeights),BinaryClassification)
  #barplot(DeNorm.Error,main = "L1 Error :Question 3 spam",xlab = "mean loss value",beside = TRUE)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)



  #Question: 4
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2penalties(TrainingData, TrainingLabels,Penalty.Vector)

  DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],W.Matrix,BinaryClassification)
  print(DeNorm.Error)
  barplot(DeNorm.Error,main = "Q4 penalty.vector Error: spam",xlab = "mean loss value",beside = TRUE)



  #Question:5
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2CV(TrainingData, TrainingLabels,Penalty.Vector)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)
}
#Linear_prostate()

#ElemStatLearn::ozone [111 x 3] output is first column (ozone)
Linear_ozone<-function()
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


  #Question:1
  #DeNormalizedWeights <- LMSquareLossIterations(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],30,0.1)



  #Question:2
  #DeNormalizedWeights<-LMSquareLossEarlyStoppingCV(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol], fold.vec,Folds,30)
  #ES.List <-LMSquareLossEarlyStoppingCV(TrainingData,TrainingLabels, fold.vec,Folds,30)
  #print(ES.List)
  #DeNormalizedWeights <- ES.List$w.mat


  #Questions: (3)
  #Penalty.Vector <-array(rep(0.01,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)))


  #Initial.W.Vector<-array(rep(0,NCOL(TrainingData)),dim=c(1,NCOL(TrainingData)+1))

  #print("Initial weight vector")
  #print(dim(Initial.W.Vector))

  #TrainingData.mean <- mean(TrainingData)
  #TrainingData.Sum = 0

  #for( row in 1:nrow(TrainingData))
  #{
  #  TrainingData.Sum = TrainingData.Sum + sum((TrainingData[row,] - TrainingData.mean)^2)
  #}

  #get sd from the calculated sum and number of observations
  #TrainingData.sd = sqrt(TrainingData.Sum / length(TrainingData))
  #Normalized_TrainingData <- data.matrix(NormalizeMatrix(TrainingData))

  #Question: 3 Function call
  #Penalty.Scalar=2
  #W.Matrix <-LMSquareLossL2(Normalized_TrainingData,  TrainingLabels, Penalty.Scalar, .31,Initial.W.Vector)
  #LMSquareLossL2(Normalized_TrainingData, TrainingLabels, penalty, opt.thresh, initial.weight.vec)

  #print(dim(W.Matrix))

  #DeNormalizedWeights<-(t(W.Matrix))/TrainingData.sd

  #print(ES.List$)

  #print("DIM of DeNormalizedWeights")
  #print(dim(DeNormalizedWeights))
  #print(DeNormalizedWeights)

  #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],DeNormalizedWeights,BinaryClassification)
  #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],t(DeNormalizedWeights),BinaryClassification)
  #barplot(DeNorm.Error,main = "L1 Error :Question 3 spam",xlab = "mean loss value",beside = TRUE)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)



  #Question: 4
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2penalties(TrainingData, TrainingLabels,Penalty.Vector)

  DeNorm.Error <-Find_Wmatrix_MeanL1Error(Cliped[,DataColsStart:DataColsEnd], Cliped[,LabelCol],W.Matrix,BinaryClassification)
  print(DeNorm.Error)
  barplot(DeNorm.Error,main = "Q4 penalty.vector Error: spam",xlab = "mean loss value",beside = TRUE)



  #Question:5
  Penalty.Vector <-array(seq(1, 0.1, by=-0.1),dim=c(1,10))
  W.Matrix<- LMSquareLossL2CV(TrainingData, TrainingLabels,Penalty.Vector)

  #print("DIM of DeNorm.Error")
  #print(dim(DeNorm.Error))
  #print(DeNorm.Error)
  #barplot(DeNorm.Error,main = "LM SquareLoss:Question 3 spam",xlab = "mean loss value",beside = TRUE)
}
#Linear_ozone()
