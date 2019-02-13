library(Rcpp)
library(rbenchmark)

#include local files
sourceCpp("../src/KNN.cpp")



x <- rnorm(5)
SortVector(x)
#TODO Fix Error Expressions
Test_TrainingData<-function(Training.Input, Training.Label)
{
  if(length(TrainingInput) == length(TrainingLabel))
  {
    print(length(TrainingInput))
    print(length(TrainingLabel))
    stop("( The Rows for Both (Parameter(0)) Training.Input, and (Parameter(0)) Training.Label, must be the same Length")
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


#Defined R's KNN implementation

NN1toKmaxPredict <- function(Training.Input, Training.Label, Neighbors, TestInput, TestPrediction)
{
  #write type/dimension checking code in the beginning of that function, and stop()
  # with an informative error message if there are any issues.
  Test_TrainingData(Training.Input,Training.Label)
  if(n.folds == 0){
    stop("( Parameter(4)) 'n.folds' contains novalue, \n
         and should contain the number of Folds to preform")
  }
  if(fold.vec == NULL){
    print("Warning: ( Parameter(3)) 'fold.vec' contained no value \n
         Should Contain a vector of fold ID numbers")
    fold.vec <-Random_Folds(length(Y.vec),5)
  }



  NN1toKmaxPredict(
    as.double,
    )
}






#Example how to properly cast variables to C code
.c("FunctionInterface",
    as.double(),
    as.double(),
    as.double())
