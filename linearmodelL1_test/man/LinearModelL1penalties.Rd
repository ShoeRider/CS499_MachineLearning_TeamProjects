% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Temp2.R
\name{LinearModelL1penalties}
\alias{LinearModelL1penalties}
\title{Linear Model with L1 regularization}
\usage{
LinearModelL1penalties(X.mat, y.vec, penalty.vec, step.size,
  opt.thresh = 1200, max.iterations = 1000)
}
\arguments{
\item{Normalized_TrainingData}{numeric imput feature matrix [n x p]}

\item{TrainingLabels}{numberic input label vector [n]
either all 0/1 for binary classification or other real numbers for regression}

\item{Iterations}{integer that determines the number of steps taken to find the optimal}

\item{StepSize.Scalar}{scalar integer, determines the size of each step.}
}
\value{
result.list <- list(
L1Error.vector = L1Error.vector,
selected.steps = selected.steps,
weight.mat = W.Matrix, [n_features+1 x n_penalties], weight matrix on original scale, that can be used to get predictions via cbind(1, X.mat) %*% W.mat (the first row of W.mat should be the bias/beta/intercept)
weight.vec = as.vector(Best.W)  [n_features+1 x 1]
)
}
\description{
This algorithm takes the penalty vector as input and iterate through each value in the vector using LinearModelL1
}
\examples{
 Spam<-Prep_Spam()
 folds.n = 4L
 Scalar.Step = 0.4
 max.iterations = 3000L
 fold.vec = as.double(sample(1:(4L),Spam$n_Elements,replace=T))
 Scaled.Train  = scale(Spam$TrainingData)
 Initial.Vector <- array(as.matrix(rep(0,NCOL(Scaled.Train)+1)),dim=c(1,NCOL(Scaled.Train)+1))
 y.vec <- as.numeric(Spam$TrainingLabels)

List = LinearModelL1penalties(Scaled.Train, y.vec,penalty.vec = seq(1, 0.1, -0.1),step.size = 0.1,opt.thresh=300,max.iterations=1000)
W.Matrix  = List$weight.mat
}
