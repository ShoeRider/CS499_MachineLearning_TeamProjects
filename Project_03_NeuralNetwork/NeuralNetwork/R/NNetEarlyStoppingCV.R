#R Function name: NNetEarlyStoppingCV

#Inputs: X.mat, y.vec, fold.vec, max.iterations, step.size, n.hidden.units.
#should use K-fold cross-validation based on the fold IDs provided in fold.vec
#for each train/validation split, use NNetIterations to compute the predictions for all observations
#compute mean.validation.loss.vec, which is a vector (with max.iterations elements) of mean validation loss over all K folds (use the square loss for regression and the 01-loss for binary classification).
#compute mean.train.loss.vec, analogous to above but for the train data.
#minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
#finally use NNetIterations(max.iterations=selected.steps) on the whole training data set.
#Output the same list from NNetIterations and add
#mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
#selected.steps

#' NNetEarlyStoppingCV
#'
#'
#'
#'@param X.mat numeric imput feature matrix [n x p],
#'@param y.vec numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param fold.vec
#'@param max.iterations
#'@param step.size
#'@param n.hidden.units
#'
#'@return
#'@export
#'
#'@examples
#'~~ Example 1 ~~
#'~~ Example 2 ~~
NNetEarlyStoppingCV<-function(X.mat, Y.vec, fold.vec, max.iterations=30, step.size, n.hidden.units)
{}
