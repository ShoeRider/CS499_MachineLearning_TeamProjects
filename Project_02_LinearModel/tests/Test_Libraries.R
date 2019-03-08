# to use these tests include these commands:
if (!require("ElemStatLearn")) install.packages("ElemStatLearn")
if (!require("testthat")) install.packages("testthat")
#library(testthat)
library(ElemStatLearn)

context("LinearModel")





#This function works better with larger lists!!
#note RandomNumbers generate (0,Folds)
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  sample(1:(Folds),Size,replace=T)
}



#Binary Tests

Prep_Spam<-function()
{
  Folds <- 4
  MaxNeighbors <- 30
  Iterations <-30
  BinaryClassification = 1

  Local_spam<- ElemStatLearn::spam

  Local_spam$spam <- sapply(as.character(Local_spam$spam),switch,"spam"=1,"email"=0)
  MaxSample_ofType = length(Local_spam[Local_spam$spam == 1,][,1])


  Spam <- data.frame(Local_spam[Local_spam$spam == 1,])


  email = Local_spam[0,]
  email <- head(Local_spam[Local_spam$spam == 0,],MaxSample_ofType)
  #print(NROW(email))

  Cliped<-rbind(Spam,email)
  Cliped<-Cliped[sample(nrow(Cliped)),]
  DataColsStart = 0
  DataColsEnd   = NCOL(Cliped) - 1
  LabelCol      = NCOL(Cliped)
  Rows          = NROW(Cliped)


  TrainingLabels <- as.matrix(Cliped[,LabelCol])
  TrainingData   <- as.matrix(Cliped[,DataColsStart:DataColsEnd])
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    TrainingData = TrainingData,
    TrainingLabels = TrainingLabels,
    Folds.Vec = Random_Folds(Rows,Folds),
    Folds.n = Folds,
    Iterations = Iterations,
    Penalty.Vector = Penalty.Vector,
    Initial.Vector = Initial.Vector,
    BinaryClassification = BinaryClassification
  )
}

#Spam<-Prep_Spam()
#print(Spam["Folds"])
#print(Spam["TrainingLabels"])

Prep_SAheart<-function()
{
  Folds              <- 4
  MaxNeighbors       <- 30
  Iterations         <-30

  #output is first column,
  #  and ignore classes other than 0 and 1
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


  TrainingData   <- as.matrix(Local_SAheart[,DataColsStart:DataColsEnd])
  TrainingLabels <- as.matrix(Local_SAheart[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    TrainingData = TrainingData,
    TrainingLabels = TrainingLabels,
    Folds.Vec = Random_Folds(Rows,Folds),
    Folds.n = Folds,
    Iterations = Iterations,
    Penalty.Vector = Penalty.Vector,
    Initial.Vector = Initial.Vector,
    BinaryClassification = BinaryClassification
  )
}

Prep_Ziptrain<-function()
{
  Folds                <- 3
  MaxNeighbors         <- 30
  Iterations           <- 30
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



  TrainingData   <- as.matrix(Local_ZipTrain[,DataColsStart:DataColsEnd])
  TrainingLabels <- as.matrix(Local_ZipTrain[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    TrainingData = TrainingData,
    TrainingLabels = TrainingLabels,
    Folds.Vec = Random_Folds(Rows,Folds),
    Folds.n = Folds,
    Iterations = Iterations,
    Penalty.Vector = Penalty.Vector,
    Initial.Vector = Initial.Vector,
    BinaryClassification = BinaryClassification
  )
}

Prep_Prostate<-function()
{
  Folds                <- 4
  MaxNeighbors         <- 30
  Iterations           <- 30
  Local_prostate<- ElemStatLearn::prostate[,1:9]


  DataColsStart = 2
  DataColsEnd   = length(Local_prostate[1,]) -1
  LabelCol      = length(Local_prostate[1,])
  Rows          = length(Local_prostate[,1])


  TrainingData   <- as.matrix(Local_prostate[,DataColsStart:DataColsEnd])
  TrainingLabels <- as.matrix(Local_prostate[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    TrainingData = TrainingData,
    TrainingLabels = TrainingLabels,
    Folds.Vec = Random_Folds(Rows,Folds),
    Folds.n = Folds,
    Iterations = Iterations,
    Penalty.Vector = Penalty.Vector,
    Initial.Vector = Initial.Vector,
    BinaryClassification = BinaryClassification
  )
}


Prep_Ozone<-function()
{
  Folds <- 3
  MaxNeighbors <- 30
  Iterations   <- 30
  Local_ozone<- ElemStatLearn::ozone


  DataColsStart = 2
  DataColsEnd   = NCOL(Local_ozone)
  LabelCol      = 1
  Rows          = NROW(Local_ozone)


  TrainingData   <- as.matrix(Local_ozone[,DataColsStart:DataColsEnd])
  TrainingLabels <- as.matrix(Local_ozone[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    TrainingData = TrainingData,
    TrainingLabels = TrainingLabels,
    Folds.Vec = Random_Folds(Rows,Folds),
    Folds.n = Folds,
    Iterations = Iterations,
    Penalty.Vector = Penalty.Vector,
    Initial.Vector = Initial.Vector,
    BinaryClassification = BinaryClassification
  )
}




#Tests themselves
Linear_Spam_Tests<-function()
{
  Spam<-Prep_Spam()

  #Question:1
  if(FALSE)
  {
    print("Linear_Spam_Tests: Question:1")
    Scalar.Step = 0.1
    DeNormalizedWeights <- LMSquareLossIterations(Spam$TrainingData, Spam$TrainingLabels,Spam$Iterations,Scalar.Step)
    #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Spam[""], Spam[],(DeNormalizedWeights),Spam$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Spam$TrainingData, Spam$TrainingLabels,as.matrix(DeNormalizedWeights),SAheart$BinaryClassification)
    print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 1: LMSquareLossIterations:Spam",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question:2
  if(FALSE)
  {
   print("Linear_Spam_Tests: Question:2")
   ES.List <-LMSquareLossEarlyStoppingCV(Spam$TrainingData, Spam$TrainingLabels,Spam$Folds.Vec,Spam$Folds.n,Spam$Iterations)
   print(ES.List)
   DeNormalizedWeights <- ES.List$w.mat
   DeNorm.Error <-Find_Wmatrix_MeanL1Error(Spam$TrainingData, Spam$TrainingLabels,(DeNormalizedWeights),Spam$BinaryClassification)
   barplot(DeNorm.Error,main = "Question 2: LMSquareLossEarlyStoppingCV:Spam",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }

  #Questions: (3)
  if(FALSE)
  {
    print("Linear_Spam_Tests: Question:3")
    Normalized_TrainingData_List <- NormalizeMatrix_List(Spam$TrainingData)
    Penalty.Scalar = 2
    opt.thresh = .3

    W.Matrix <-LMSquareLossL2(Normalized_TrainingData_List$NormalizedMatrix,  Spam$TrainingLabels, Penalty.Scalar, opt.thresh,Spam$Initial.Vector)
    DeNormalized.W.Matrix <-(t(W.Matrix))/Normalized_TrainingData_List$sd

    #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Spam$TrainingData, Spam$TrainingLabels,t(DeNormalized.W.Matrix),Spam$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Spam$TrainingData, Spam$TrainingLabels,t(DeNormalized.W.Matrix),Spam$BinaryClassification)

    #Lame Graph but what are we to do?
    barplot(DeNorm.Error,main = "Question 3: LMSquareLossL2:Spam",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question: 4
  if(FALSE)
  {
    print("Linear_Spam_Tests: Question:4")
    W.Matrix<- LMSquareLossL2penalties(Spam$TrainingData, Spam$TrainingLabels, Spam$Penalty.Vector)

    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Spam$TrainingData, Spam$TrainingLabels,W.Matrix,Spam$BinaryClassification)
    #print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 4: LMSquareLossL2penalties:Spam",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }



  #Question:5
  if(TRUE)
  {
    print("Linear_Spam_Tests: Question:5")
    LMSquareLossL2CV_List <- LMSquareLossL2CV(Spam$TrainingData, Spam$TrainingLabels,Spam$Folds.Vec, Spam$Penalty.Vector)

    DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
    barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :Spam",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }
}
#Linear_Spam_Tests()


#ElemStatLearn::SAheart 2-class [462, 9] output is last column (chd).
Linear_SAheart_Test<-function()
{
  SAheart<-Prep_SAheart()

  print(SAheart$BinaryClassification)

  #Question:1
  if(TRUE)
  {
    print("Linear_SAheart_Tests: Question:1")
    Scalar.Step = 0.005
    DeNormalizedWeights <- LMSquareLossIterations(SAheart$TrainingData, SAheart$TrainingLabels,SAheart$Iterations,Scalar.Step)
    #DeNorm.Error <-Find_Wmatrix_MeanL1Error(SAheart[""], SAheart[],(DeNormalizedWeights),SAheart$BinaryClassification)
    if(FALSE)
    {
      DeNorm.Error <-Find_Wmatrix_MeanL1Error(SAheart$TrainingData, SAheart$TrainingLabels,as.matrix(DeNormalizedWeights),SAheart$BinaryClassification)
      print(DeNorm.Error)
      barplot(DeNorm.Error,main = "Question 1: LMSquareLossIterations:SAheart",xlab = "Iteration",ylab = "Error",beside = TRUE)
    }
    if(TRUE)
    {
      DeNorm.Error <-Find_Wmatrix_MeanL2Error(SAheart$TrainingData, SAheart$TrainingLabels,as.matrix(DeNormalizedWeights),SAheart$BinaryClassification)
      print(DeNorm.Error)
      barplot(DeNorm.Error,main = "Question 1: Find_Wmatrix_MeanL2Error:SAheart",xlab = "Iteration",ylab = "Error",beside = TRUE)
    }

  }


  #Question:2
  if(FALSE)
  {
    print("Linear_SAheart_Tests: Question:2")
    ES.List <-LMSquareLossEarlyStoppingCV(SAheart$TrainingData, SAheart$TrainingLabels,SAheart$Folds.Vec,SAheart$Folds.n,SAheart$Iterations)
    print(ES.List)
    DeNormalizedWeights <- ES.List$w.mat
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(SAheart$TrainingData, SAheart$TrainingLabels,(DeNormalizedWeights),SAheart$BinaryClassification)
    barplot(DeNorm.Error,main = "Question 2: LMSquareLossEarlyStoppingCV:SAheart",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }

  #Questions: (3)
  if(FALSE)
  {
    print("Linear_SAheart_Tests: Question:3")
    Normalized_TrainingData_List <- NormalizeMatrix_List(SAheart$TrainingData)
    Penalty.Scalar = 2
    opt.thresh = .3

    W.Matrix <-LMSquareLossL2(Normalized_TrainingData_List$NormalizedMatrix,  SAheart$TrainingLabels, Penalty.Scalar, opt.thresh,SAheart$Initial.Vector)
    DeNormalized.W.Matrix <-(t(W.Matrix))/Normalized_TrainingData_List$sd

    #DeNorm.Error <-Find_Wmatrix_MeanL2Error(SAheart$TrainingData, SAheart$TrainingLabels,t(DeNormalized.W.Matrix),SAheart$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(SAheart$TrainingData, SAheart$TrainingLabels,t(DeNormalized.W.Matrix),SAheart$BinaryClassification)

    #Lame Graph but what are we to do?
    barplot(DeNorm.Error,main = "Question 3: LMSquareLossL2:SAheart",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question: 4
  if(FALSE)
  {
    print("Linear_SAheart_Tests: Question:4")
    W.Matrix<- LMSquareLossL2penalties(SAheart$TrainingData, SAheart$TrainingLabels, SAheart$Penalty.Vector)

    DeNorm.Error <-Find_Wmatrix_MeanL1Error(SAheart$TrainingData, SAheart$TrainingLabels,W.Matrix,SAheart$BinaryClassification)
    #print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 4: LMSquareLossL2penalties:SAheart",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }



  #Question:5
  if(FALSE)
  {
    print("Linear_SAheart_Tests: Question:5")
    LMSquareLossL2CV_List <- LMSquareLossL2CV(SAheart$TrainingData, SAheart$TrainingLabels,SAheart$Folds.Vec, SAheart$Penalty.Vector)

    DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
    barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :SAheart",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }
}
#Linear_SAheart_Test()



#ElemStatLearn::zip.train: 10-class [7291, 256] output is first column. (ignore classes other than 0 and 1)
Linear_Ziptrain_Test<-function()
{
  Ziptrain<-Prep_Ziptrain()

  #Question:1
  if(FALSE)
  {
    print("Linear_Ziptrain_Tests: Question:1")
    Scalar.Step = 0.1
    DeNormalizedWeights <- LMSquareLossIterations(Ziptrain$TrainingData, Ziptrain$TrainingLabels,Ziptrain$Iterations,Scalar.Step)

    if(TRUE)
    {
      #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ziptrain[""], Ziptrain[],(DeNormalizedWeights),Ziptrain$BinaryClassification)
      DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ziptrain$TrainingData, Ziptrain$TrainingLabels,as.matrix(DeNormalizedWeights),Ziptrain$BinaryClassification)
      #print(DeNorm.Error)
      barplot(DeNorm.Error,main = "Question 1: LMSquareLossIterations:Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
    }
    if(FALSE)
    {
      #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ziptrain[""], Ziptrain[],(DeNormalizedWeights),Ziptrain$BinaryClassification)
      DeNorm.Error <-Find_Wmatrix_MeanL2Error(Ziptrain$TrainingData, Ziptrain$TrainingLabels,as.matrix(DeNormalizedWeights),Ziptrain$BinaryClassification)
      #print(DeNorm.Error)
      barplot(DeNorm.Error,main = "Question 1: LMSquareLossIterations:Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
    }

  }


  #Question:2
  if(FALSE)
  {
    print("Linear_Ziptrain_Tests: Question:2")
    ES.List <-LMSquareLossEarlyStoppingCV(Ziptrain$TrainingData, Ziptrain$TrainingLabels,Ziptrain$Folds.Vec,Ziptrain$Folds.n,Ziptrain$Iterations)
    print(ES.List)
    DeNormalizedWeights <- ES.List$w.mat
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ziptrain$TrainingData, Ziptrain$TrainingLabels,(DeNormalizedWeights),Ziptrain$BinaryClassification)
    barplot(DeNorm.Error,main = "Question 2: LMSquareLossEarlyStoppingCV:Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }

  #Questions: (3)
  if(FALSE)
  {
    print("Linear_Ziptrain_Tests: Question:3")
    Normalized_TrainingData_List <- NormalizeMatrix_List(Ziptrain$TrainingData)
    Penalty.Scalar = 2
    opt.thresh = .3

    W.Matrix <-LMSquareLossL2(Normalized_TrainingData_List$NormalizedMatrix,  Ziptrain$TrainingLabels, Penalty.Scalar, opt.thresh,Ziptrain$Initial.Vector)
    DeNormalized.W.Matrix <-(t(W.Matrix))/Normalized_TrainingData_List$sd

    #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Ziptrain$TrainingData, Ziptrain$TrainingLabels,t(DeNormalized.W.Matrix),Ziptrain$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ziptrain$TrainingData, Ziptrain$TrainingLabels,t(DeNormalized.W.Matrix),Ziptrain$BinaryClassification)

    #Lame Graph but what are we to do?
    barplot(DeNorm.Error,main = "Question 3: LMSquareLossL2:Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question: 4
  if(FALSE)
  {
    print("Linear_Ziptrain_Tests: Question:4")
    W.Matrix<- LMSquareLossL2penalties(Ziptrain$TrainingData, Ziptrain$TrainingLabels, Ziptrain$Penalty.Vector)

    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ziptrain$TrainingData, Ziptrain$TrainingLabels,W.Matrix,Ziptrain$BinaryClassification)
    #print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 4: LMSquareLossL2penalties:Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }



  #Question:5
  if(FALSE)
  {
    print("Linear_Ziptrain_Tests: Question:5")
    LMSquareLossL2CV_List <- LMSquareLossL2CV(Ziptrain$TrainingData, Ziptrain$TrainingLabels,Ziptrain$Folds.Vec, Ziptrain$Penalty.Vector)

    DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
    barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }
}
#Linear_Ziptrain_Test()



#Regression.
#ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
Linear_Prostate_Test<-function()
{
  Prostate<-Prep_Prostate()

  #Question:1
  if(FALSE)
  {
    print("Linear_Prostate_Tests: Question:1")
    Scalar.Step = 0.02
    DeNormalizedWeights <- LMSquareLossIterations(Prostate$TrainingData, Prostate$TrainingLabels,Prostate$Iterations,Scalar.Step)
    #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Prostate[""], Prostate[],(DeNormalizedWeights),Prostate$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Prostate$TrainingData, Prostate$TrainingLabels,as.matrix(DeNormalizedWeights),Prostate$BinaryClassification)
    print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 1: LMSquareLossIterations:Prostate",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question:2
  if(FALSE)
  {
    print("Linear_Prostate_Tests: Question:2")
    ES.List <-LMSquareLossEarlyStoppingCV(Prostate$TrainingData, Prostate$TrainingLabels,Prostate$Folds.Vec,Prostate$Folds.n,Prostate$Iterations)
    print(ES.List)
    DeNormalizedWeights <- ES.List$w.mat
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Prostate$TrainingData, Prostate$TrainingLabels,(DeNormalizedWeights),Prostate$BinaryClassification)
    barplot(DeNorm.Error,main = "Question 2: LMSquareLossEarlyStoppingCV:Prostate",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }

  #Questions: (3)
  if(FALSE)
  {
    print("Linear_Prostate_Tests: Question:3")
    Normalized_TrainingData_List <- NormalizeMatrix_List(Prostate$TrainingData)
    Penalty.Scalar = 2
    opt.thresh = .3

    W.Matrix <-LMSquareLossL2(Normalized_TrainingData_List$NormalizedMatrix,  Prostate$TrainingLabels, Penalty.Scalar, opt.thresh,Prostate$Initial.Vector)
    DeNormalized.W.Matrix <-(t(W.Matrix))/Normalized_TrainingData_List$sd

    #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Prostate$TrainingData, Prostate$TrainingLabels,t(DeNormalized.W.Matrix),Prostate$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Prostate$TrainingData, Prostate$TrainingLabels,t(DeNormalized.W.Matrix),Prostate$BinaryClassification)

    #Lame Graph but what are we to do?
    barplot(DeNorm.Error,main = "Question 3: LMSquareLossL2:Prostate",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question: 4
  if(FALSE)
  {
    print("Linear_Prostate_Tests: Question:4")
    W.Matrix<- LMSquareLossL2penalties(Prostate$TrainingData, Prostate$TrainingLabels, Prostate$Penalty.Vector)

    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Prostate$TrainingData, Prostate$TrainingLabels,W.Matrix,Prostate$BinaryClassification)
    #print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 4: LMSquareLossL2penalties:Prostate",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }



  #Question:5
  if(TRUE)
  {
    print("Linear_Prostate_Tests: Question:5")
    LMSquareLossL2CV_List <- LMSquareLossL2CV(Prostate$TrainingData, Prostate$TrainingLabels,Prostate$Folds.Vec, Prostate$Penalty.Vector)

    DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
    barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :Prostate",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }
}
Linear_Prostate_Test()


#ElemStatLearn::ozone [111 x 3] output is first column (ozone)
Linear_Ozone_Test<-function()
{
  Ozone<-Prep_Ozone()

  #Question:1
  if(FALSE)
  {
    print("Linear_Ozone_Tests: Question:1")
    Scalar.Step = 0.02
    DeNormalizedWeights <- LMSquareLossIterations(Ozone$TrainingData, Ozone$TrainingLabels,Ozone$Iterations,Scalar.Step)
    #DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ozone[""], Ozone[],(DeNormalizedWeights),Ozone$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ozone$TrainingData, Ozone$TrainingLabels,as.matrix(DeNormalizedWeights),Ozone$BinaryClassification)
    print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 1: LMSquareLossIterations:Ozone",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question:2
  if(FALSE)
  {
    print("Linear_Ozone_Tests: Question:2")
    ES.List <-LMSquareLossEarlyStoppingCV(Ozone$TrainingData, Ozone$TrainingLabels,Ozone$Folds.Vec,Ozone$Folds.n,Ozone$Iterations)
    print(ES.List)
    DeNormalizedWeights <- ES.List$w.mat
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ozone$TrainingData, Ozone$TrainingLabels,(DeNormalizedWeights),Ozone$BinaryClassification)
    barplot(DeNorm.Error,main = "Question 2: LMSquareLossEarlyStoppingCV:Ozone",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }

  #Questions: (3)
  if(FALSE)
  {
    print("Linear_Ozone_Tests: Question:3")
    Normalized_TrainingData_List <- NormalizeMatrix_List(Ozone$TrainingData)
    Penalty.Scalar = 2
    opt.thresh = .3

    W.Matrix <-LMSquareLossL2(Normalized_TrainingData_List$NormalizedMatrix,  Ozone$TrainingLabels, Penalty.Scalar, opt.thresh,Ozone$Initial.Vector)
    DeNormalized.W.Matrix <-(t(W.Matrix))/Normalized_TrainingData_List$sd

    #DeNorm.Error <-Find_Wmatrix_MeanL2Error(Ozone$TrainingData, Ozone$TrainingLabels,t(DeNormalized.W.Matrix),Ozone$BinaryClassification)
    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ozone$TrainingData, Ozone$TrainingLabels,t(DeNormalized.W.Matrix),Ozone$BinaryClassification)

    #Lame Graph but what are we to do?
    barplot(DeNorm.Error,main = "Question 3: LMSquareLossL2:Ozone",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }


  #Question: 4
  if(FALSE)
  {
    print("Linear_Ozone_Tests: Question:4")
    W.Matrix<- LMSquareLossL2penalties(Ozone$TrainingData, Ozone$TrainingLabels, Ozone$Penalty.Vector)

    DeNorm.Error <-Find_Wmatrix_MeanL1Error(Ozone$TrainingData, Ozone$TrainingLabels,W.Matrix,Ozone$BinaryClassification)
    #print(DeNorm.Error)
    barplot(DeNorm.Error,main = "Question 4: LMSquareLossL2penalties:Ozone",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }



  #Question:5
  if(TRUE)
  {
    print("Linear_Ozone_Tests: Question:5")
    LMSquareLossL2CV_List <- LMSquareLossL2CV(Ozone$TrainingData, Ozone$TrainingLabels,Ozone$Folds.Vec, Ozone$Penalty.Vector)

    DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
    barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :Ozone",xlab = "Iteration",ylab = "Error",beside = TRUE)
  }
}
Linear_Ozone_Test()




#TODO: Fix implementation
Test_Norm_DeNorm_Function<-function()
{
  Spam = Prep_Spam()
  Norm_List = COL_NormalizeMatrix_List(Spam$TrainingData)
  print(COL_DeNormalizeMatrix_List(Norm_List))
  print(Spam$TrainingData)
  print(sum(COL_DeNormalizeMatrix_List(Norm_List)) - sum(Spam$TrainingData))
}
#Test_Norm_DeNorm_Function()


