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
#'~~ Example 1 ~~ Regression
#'=========================================================================================
#'data(ozone, package = "ElemStatLearn")
#'iterations <- 20
#'n.hidden.units <- 5
#'step.size <- .1
#'Labels <- ozone[,1] # first col is output of the data
#'Data <- ozone[,2:ncol(ozone)]
#'
#'TrainingData <- as.matrix(Data)[1:57,]
#'TestData <- as.matrix(Data)[58:ncol(TrainingData),]
#'
#'TrainingLabels <- Labels[1:57]
#'TestLabels <- Labels[58:length(Labels)]
#'is.train.vec <- sample(c(TRUE,FALSE),replace = TRUE, size = length(TrainingLabels))
#'
#'output <- NNetIterations(TrainingData,TrainingLabels,
#'                         iterations,step.size,n.hidden.units,is.train.vec)
#'===========================================================================================
#'~~ Example 2 ~~
NNetIterations<-function(X.mat, Y.vec, max.iterations=30, step.size, n.hidden.units=10,is.train)
{
  # assuming that X.mat is already scaled properly
  # first check input

  # TODO: must break data into train and test and fix all references to the five X.mat

  # verify the size of the label vec and the X.mat
  if(ncol(X.mat) == length(Y.vec)){
    stop("Error: X.mat columns does not match the length of the Y.vec")
  }
  # check and catch null data in the training set
  if(is.null(any(X.mat)) || is.null(any(Y.vec))){
    stop("Error: Null data was encountered in the training data")
  }
  # checks for valis iterations
  if(max.iterations < 1 || max.iterations > 1000 ){
    stop("Error: Invalid Iterations Value")
  }
  # check the train vec against the y.vec for size
  if(length(is.train) != length(Y.vec)){
    stop("Error: is.train is the wrong dimensions")
  }

  # check the is.train vec has valid contents
  if(is.numeric(is.train)){
    stop("Error: is.train has intergers in the vector contents")
  }
  # check the is.train vec has valid contents
  if(!(all(is.train) %in% c(TRUE,FALSE))){
    stop("Error: is.train has wrong contents")
  }

  # decide if its regression or binary
  show(Y.vec)
  if(all(Y.vec %in% c(0,1))){
    is.binary <- TRUE
  }else{
    is.binary <- FALSE
    }
  # show(is.binary)

  # break apart the train and the validation sets
  returnList <- breakData(x.mat, y.vec, is.train)

  train.loss.vec <- rep(0,max.iterations)

  # TODO ASK ABOUT THE NA VALUES THAT WILL BE RETRIEVED FROM THE STD

  # row 1 will be all the colmeans and row 2 will be all col std
  sd.vec <- rep(0,ncol = ncol(X.mat))

  # get col means
  means.vec <- colMeans(X.mat)

  # stores the features at which the sd is 0,
  #         thus these are the indeces needed to fix the V.mat before return
  #   vals will be 0 if that column has sd = 0 and 1 otherwise
  zero.sd.vec <- rep(0,ncol = ncol(X.mat))

  # compute and store the mean and sd for each row to return the matrix at the origional scale
  for (col in seq(1,ncol(X.mat))){
    sd.vec[col] <- sd(X.mat[,col])
    if(sd.vec[col] == 0){
      zero.sd.vec[col] = 1
    }
    else if(is.na(sd.vec[col])){
      zero.sd.vec[col] = 0
    }
  } # end for loop

  # remove nulls from both the y.vec and the final V.mat output matrix
  # show(as.numeric(zero.sd.vec))
  # save the mean and std of each col
  rescale.mat <- rbind(means.vec, sd.vec)
  #show(rescale.mat)

  # scale the matrix down
  scaled.x.mat <- scale(X.mat)
  V.mat <- matrix(0,nrow= ncol(scaled.x.mat)+1, ncol= n.hidden.units)

  # seed for testing reasons
  set.seed(20)

  # create the V vector which starts as close to all 0s as possible
  #          cannot be 0 because the gradient will fail to converge
  V <- matrix(rnorm(ncol(scaled.x.mat) * n.hidden.units), ncol(scaled.x.mat), n.hidden.units)
  # show(V)
  # create weigth vec
  w <- c(rnorm(n.hidden.units))
  # show(w)


  # start gradient descent
  for (iteration in seq(1,max.iterations)) {

    # get value for A [observations x hidden.units]
    A <- scaled.x.mat %*% V
    # show(A)
    # get the Z vector and deriv.A
    Z <- sigmoid(A)
    deriv.A <- Z * (1-Z)
    # show(deriv.A)

    # store the b value
    b <- as.numeric(Z %*% w)
    # show(b)
    # calculate delta w
    if(is.binary){
      # use the binary alg
      # this will need to be change to the -Y.train
      del.w <- -Y.vec * sigmoid(-Y.vec * b)
    }
    else{
      # use the regression alg
      # TODO: make this a train vec reference not a Y.vec
      del.w <- b - Y.vec
      # show(del.w)
    }

    del.V <- diag(del.w) %*% deriv.A %*% diag(w)

    # calculate gradient parts from in class algorithm
    gradient.w <- t(Z) %*% del.w/nrow(scaled.x.mat)
    # show(gradient.w)
    gradient.v <- t(scaled.x.mat) %*% del.V/nrow(scaled.x.mat)

    # step in the right direction
    w <- as.numeric(w - step.size * gradient.w)
    V <- V - step.size * gradient.v

    predict.sc <- function(X.tilda){
      A.mat <- X.tilda %*% V
      sigmoid(A.mat) %*% w
    }

    predict.1.orig <- function(X.unsc){
      X.tilda <- scale(
        X.unsc, attr(scaled.x.mat, "scaled:center"),attr(scaled.x.mat, "scaled:scale")
      )
      predict.sc(X.tilda)
    }

    predict.2.orig <- function(X.unsc){
      A.mat <- cbind(1,X.unsc) %*% rbind(as.numeric(b.orig), V.orig)
      sigmoid(A.mat) %*% w
    }

    # show(sum(abs(c(gradient.w,as.numeric(gradient.v)))))
    train.loss.vec[iteration] <- sum(abs(c(gradient.w,as.numeric(gradient.v))))
    # show(train.loss.vec)


  }

  # create the return V.mat
  V.mat <- rbind(1,V.mat) %*% w

  pred.mat <- predict.sc(scaled.x.mat)
  print('Predictions')
  show(pred.mat)

  # TODO: use this valiable to plot the lines later
  print("TRAIN LOSS VECTOR TO BE PLOTTED FOR REPORT")
  show(train.loss.vec)
  print("LAST V.mat that was calculated")
  rownames(V) <- NULL
  show(as.matrix(V))
  print("LAST w.vec that was calculated")
  show(w)

  # must return a list of named elements
  return(0)
}

breakData<- function(x.mat, y.vec, bool.vec){
  for (val in bool.vec){
    # train data
    if(as.logical(val) == TRUE){
    #TODO
    }else{ # validation/test data
     #TODO
    }
  }
}

sigmoid <- function(x){
  1/(1+exp(-x))
}
