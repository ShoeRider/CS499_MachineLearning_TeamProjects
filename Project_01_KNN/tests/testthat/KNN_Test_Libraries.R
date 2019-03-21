# to use these tests include these commands:
if (!require("ElemStatLearn")) install.packages("ElemStatLearn")
if (!require("testthat")) install.packages("testthat")

library(ElemStatLearn)
library(NearestNeighbors)
library(testthat)
context("knn")


print(getwd())
source("R/NNLearnCV_interface.R")
source("R/General.R")
#R CMD build 


source("tests/testthat/Prep_Libraries.R")
#source("Prep_Libraries.R")






#Binary Tests



#Tests themselves
NN1ToKmax_Spam_Tests<-function()
{
  Spam<-Prep_Spam()
  
  #Question/Requrement:1
  if(TRUE)
  {
    print("Linear_Spam_Test: Question/Requrement:1")
    Fold.vec = Random_Folds(Spam$n_Elements,4)
    Fold.n   = 4
    KNNLearnCV.List = KNNLearnCV(Spam$TrainingData, Spam$TrainingLabels, 30, Fold.vec, Fold.n)
    
    barplot(KNNLearnCV.List$train.loss.vec,main = "Spam: KNNLearnCV L2 Mean Error",xlab = "KNN Compared",ylab = "Error",beside = TRUE)
    
  }
  
  
  return(0)
}

NN1ToKmax_Spam_Tests()




