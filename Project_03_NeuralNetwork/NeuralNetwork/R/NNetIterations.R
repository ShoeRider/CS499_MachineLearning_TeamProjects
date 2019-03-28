

#NNetIterations
#
#Inputs: X.mat (feature matrix, n_observations x n_features), y.vec (label vector, n_observations x 1), max.iterations (int scalar > 1), step.size, n.hidden.units (number of hidden units), is.train (logical vector of size n_observations, TRUE if the observation is in the train set, FALSE for the validation set).
#Output: list with named elements:
#  pred.mat, n_observations x max.iterations matrix of predicted values (real number for regression, probability for binary classification).
#W.mat final weight matrix (n_features+1 x n.hidden.units)
#v.vec final weight vector (n.hidden.units+1).
#predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification). You should be able to make predictions via cbind(1, sigmoid(cbind(1, X) %*% W.mat)) %*% v.vec. The first row of W.mat should be the intercept terms; the first element of v.vec should be the intercept term.
#if all of y.vec is binary (either 0 or 1) then you should use the logistic loss, otherwise use the square loss.
#you should optimize the mean loss, which is the sum over all data divided by n_observations (if you don’t divide by the number of observations the gradient can get numerically unstable for large data).
#make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column, and keep track of the mean/sd of each column, so you can return W.mat on the original scale (if you use the unscaled X.mat during gradient descent, it will not converge – numerical instability).

#' NNetIterations
#'
#'
#'
#'@param X.mat numeric imput feature matrix [n x p],
#'@param y.vec numberic input label vector [n],
#'either all 0/1 for binary classification or other real numbers for regression
#'@param max.iterations
#'@param n.hidden.units
#'@param is.train
#'
#'@return Returns a list with the following elements
#'      X.mat              = TrainingData numeric imput feature matrix [n x p],
#'      y.vec              = TrainingLabels numberic input label vector [n],
#'      pred.mat           =
#'      Predict            = Predict(X.), Function to preform the optimal K nearest neighbors selected,
#'@export
#'
#'@examples
#'~~ Example 1 ~~
#'~~ Example 2 ~~
NNetIterations<-function(X.mat, Y.vec, max.iterations=30, step.size, n.hidden.units,is.train)
{}


