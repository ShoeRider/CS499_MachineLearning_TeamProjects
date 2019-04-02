library(NeuralNetwork)
library(testthat)
context("NormalizeMatrix")

#' checks to see if the normalize matrix function creates a matrix with
#' sd=1 mean=0 for all cols in the matrix
testthat("Testing Normalize Matrix Function",{
  data(spam,package = "ElemStatLearn")
  step.size <- 0.1
  iterations <- 20
  Local_spam<- ElemStatLearn::spam
  BinaryClassification = 1
  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])
  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])
  # print(NROW(Spam))

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

  norm.mat = NormalizeMatrix(TrainingData)
  for (col in 1:ncol(TrainingData))
    {
    expect_equal(as.numeric(sd(norm.mat[,col])),as.numeric(1))
    expect_lte(mean(norm.mat[,col]), 0) & expect_gt(mean(norm.mat[,col]), -.01) |
      expect_gte(mean(norm.mat[,col]), 0) & expect_lt(mean(norm.mat[,col]), .01)
  }

})

testthat("LogisticLossIterations Function checks for bad data",{
  data(spam, package = "ElemStatLearn")

  spam = list(
    features=as.matrix(spam[,1:57]),
    labels=ifelse(spam$spam == 'spam',1,0)
  )
  stopifnot(all(spam$labels %in% c(1,0)))
  stopifnot(length(spam$labels) == nrow(spam$features))


  LMLogisticLossIterations(spam$features,spam$labels,20,20)
})

testthat("Testing Logistic Early Stopping CV Function",{
  data(spam,package = "ElemStatLearn")
  step.size <- 0.1
  iterations <- 20
  Local_spam<- ElemStatLearn::spam
  BinaryClassification = 1
  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])
  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])
  # print(NROW(Spam))

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

  LMLogisticLossEarlyStoppingCV(TrainingData,TrainingLabels,fold.vec,5)

})


testthat("Testing Logistic L2 CV Function",{
  data(spam,package = "ElemStatLearn")
  step.size <- 0.1
  iterations <- 20
  Local_spam<- ElemStatLearn::spam
  BinaryClassification = 1
  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])
  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])
  # print(NROW(Spam))

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
  penalty.vec <- array(range(1:Rows),length(Rows))
  TrainingLabels <- data.matrix(Cliped[,LabelCol])
  TrainingData   <- data.matrix(Cliped[,DataColsStart:DataColsEnd])

  LMLogisticLossL2(NormalizeMatrix(TrainingData),TrainingLabels,fold.vec,penaly.vec)
  # should fail because of penalty vec

})


testthat("Testing LMLogisticLossL2penalties Function",{
  data(spam,package = "ElemStatLearn")
  step.size <- 0.1
  iterations <- 20
  Local_spam<- ElemStatLearn::spam
  BinaryClassification = 1
  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])
  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])
  # print(NROW(Spam))

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

  penalty.vec <- array(range(1:Rows),length(Rows))
  TrainingLabels <- data.matrix(Cliped[,LabelCol])
  TrainingData   <- data.matrix(Cliped[,DataColsStart:DataColsEnd])

  LMLogisticLossL2penalties(NormalizeMatrix(TrainingData),TrainingLabels,penaly.vec)
  # should fail because of penalty vec

})

testthat("Test LMSquareLossIterations Function",{
  data(ozone, package = "ElemStatLearn")

  iterations <- 20
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]

  output.mat <-LMSquareLossIterations(TrainingData,TrainingLabels,iterations,0.5)
  expect_equal(ncol(output.mat),iterations + 1)
})


testthat("Test LMSquareLossEarlyStoppingCV Function",{
  data(ozone, package = "ElemStatLearn")

  iterations <- 20
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  fold.vec <- Random_Folds(nrow(TrainingData),4)

  output.mat <-LMSquareLossEarlyStoppingCV(TrainingData,TrainingLabels,fold.vec,4,iterations)

})

testthat("Test LMSquareLossEarlyStoppingCV Function",{
  data(ozone, package = "ElemStatLearn")

  iterations <- 20
  Labels <- ozone[,1] # first col is output of the data
  Data <- ozone[,2:ncol(ozone)]

  TrainingData <- as.matrix(Data)[1:57,]
  TestData <- as.matrix(Data)[58:ncol(TrainingData),]

  TrainingLabels <- Labels[1:57]
  TestLabels <- Labels[58:length(Labels)]
  fold.vec <- Random_Folds(nrow(TrainingData),4)
  penalty.vec <- sort(array(c(1:20)), decreasing= TRUE)

  output.mat <-LMSquareLossL2penalties(TrainingData,TrainingLabels,penalty.vec)

})

test_that("Test LMLogistic_Gradient Function", {
  library(ElemStatLearn)
  data(zip.train, package= "ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i,-1]
  y <- zip.train[train.i, 1]

  W.vec <- array(0,length(x))
  test <- LMLogistic_Gradient(x,y,W.vec)
  expect_equal(ncol(test),nrow(as.matrix(x)))
})
