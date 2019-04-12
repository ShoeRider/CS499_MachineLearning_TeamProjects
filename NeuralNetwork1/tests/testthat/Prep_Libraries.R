#denormalize Function


Prep_Spam<-function()
{
  Folds        <- 4
  MaxNeighbors <- 30
  Iterations   <- 30

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


  TrainingLabels <- as.vector(Cliped[,LabelCol])
  TrainingData   <- as.matrix(Cliped[,DataColsStart:DataColsEnd])
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    n_Elements = Rows,
    MaxNeighbors = MaxNeighbors,
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
  TrainingLabels <- as.vector(Local_SAheart[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    n_Elements = Rows,
    MaxNeighbors = MaxNeighbors,
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

Prep_ZipTrain<-function()
{
  Folds                <- 3
  MaxNeighbors         <- 30
  Iterations           <- 30
  Temp_ZipTrain<- ElemStatLearn::zip.train
  Local_ZipTrain=c()
  for (Filter in c(0,1)){
    NonZero <- which(Temp_ZipTrain[,1]==Filter,arr.ind=TRUE)
    #print(NonZero)
    Local_ZipTrain<-rbind(Local_ZipTrain,Temp_ZipTrain[NonZero, ])
  }

  DataColsStart = 2
  DataColsEnd   = NCOL(Local_ZipTrain)
  LabelCol      = 1
  Rows          = NROW(Local_ZipTrain)

print(DataColsEnd)
print(NCOL(Temp_ZipTrain))

  TrainingData   <- as.matrix(Local_ZipTrain[,DataColsStart:DataColsEnd])
  print(NCOL(TrainingData))
  TrainingLabels <- as.vector(Local_ZipTrain[,LabelCol])



  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    n_Elements = Rows,
    MaxNeighbors = MaxNeighbors,
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
Prep_ZipTrain()

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
  TrainingLabels <- as.vector(Local_prostate[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    n_Elements = Rows,
    MaxNeighbors = MaxNeighbors,
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
  TrainingLabels <- as.vector(Local_ozone[,LabelCol])
  BinaryClassification =  all(TrainingLabels <= 1 & TrainingLabels >= 0)
  Initial.Vector <- t(as.matrix(rep(0,NCOL(TrainingData)+1),dim=c(1,NCOL(TrainingData)+1)))
  Penalty.Vector <- array(seq(1, 2, by=0.1),dim=c(1,10))



  ReturnList<-list(
    n_Elements = Rows,
    MaxNeighbors = MaxNeighbors,
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
