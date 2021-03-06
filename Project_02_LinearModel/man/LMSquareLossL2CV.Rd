% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LMLinearSquare_Functions.R
\name{LMSquareLossL2CV}
\alias{LMSquareLossL2CV}
\title{LMSquareLossL2 Cross Validation}
\usage{
LMSquareLossL2CV(TrainingData, TrainingLabels, fold.vec, penalty.vec)
}
\arguments{
\item{TrainingData}{numeric imput feature matrix [n x p]}

\item{TrainingLabels}{numberic input label vector [n],
either all 0/1 for binary classification or other real numbers for regression}

\item{fold.vec}{vector indicating the fold each element is in [n]}

\item{penalty.vec}{is a [m] vector with decreasing (non-negative numeric scalar values) penalty values, that control the L2 regularization.}
}
\value{
numeric matrix of the weight matrix at each step, from 1 to Iterations.
Returned Matrix is[n_features+1 x max.iterations], where the first element of the weight vector is be the intercept term(AKA Bias).
}
\description{
Makes iterative steps using gradient decent to find a solution to the Linear Models problem
}
\examples{
## Example: 1 With The Spam DataSet:##
 #PleaseNote the Prep_<Library>() functions are loctated in the tests/testthat folder
 Spam<-Prep_Spam()

 LMSquareLossL2CV_List <- LMSquareLossL2CV(Spam$TrainingData, Spam$TrainingLabels,Spam$Folds.Vec, Spam$Penalty.Vector)
 DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
 barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :Spam",xlab = "Iteration",ylab = "Error",beside = TRUE)

## Example: 2  With The ZipTrain Dataset:##
 #PleaseNote the Prep_<Library>() functions are loctated in the tests/testthat folder
 Ziptrain<-Prep_Ziptrain()
 LMSquareLossL2CV_List <- LMSquareLossL2CV(Ziptrain$TrainingData, Ziptrain$TrainingLabels,Ziptrain$Folds.Vec, Ziptrain$Penalty.Vector)

 DeNorm.Error <- LMSquareLossL2CV_List$mean.validation.loss
 barplot(DeNorm.Error,main = "Question 5: mean.validation.loss :Ziptrain",xlab = "Iteration",ylab = "Error",beside = TRUE)
}
