#'L1 Regularization Linear Model for regression and binary classification
#'

#' @author
#' @author
#' @author
#' @author
#' @description
#' Training by using L1 Regularization with gradient descending
#' (real numbers for regression, probabilities for binary classification).



#'@method LinearModelL1
#'@param X.scaled.mat (feature matrix, n x p)
#'@param y.vec (label vector, p x 1)
#'@param penalty (numeric scalar >= 0)
#'@param opt.thresh (positive scalar)
#'@param initial.w.vec (p x 1)
#'@param step.size (positive scalar)
#'
#'@return optimal weight vector (p+1 elements, first element is the bias/intercept b)
#'
#' @export
#'
#' @examples
#' BINARY
#' ---------------
#'data(zip.train, package="ElemStatLearn")
#'all.y.vec <- zip.train[,1]
#'is.01 <- all.y.vec %in% c(0,1)
#'y.vec <- all.y.vec[is.01]
#'X.mat <- zip.train[is.01,-1]
#'
#'penalty <- 2
#'
#'opt.thresh <- 2
#'
#'step.size <- .01
#'
#'scaled.x.mat <- scale(X.mat)
#'x.sc.filtered <- scaled.x.mat[, attr(scaled.x.mat, "scaled:scale") != 0]
#'
#'w.vec <- rep(0, l=ncol(x.sc.filtered) + 1)
#'
#'LinearModelL1(x.sc.filtered,y.vec, penalty, opt.thresh, w.vec, step.size)
#'-----------
#'
#'REGRESSION
#'-----------
#'data(zip.train, package="ElemStatLearn")
#'y.vec <- zip.train[,1]
#'X.mat <- zip.train[,-1]
#'
#'penalty <- 2
#'
#'opt.thresh <- 2
#'
#'step.size <- .01
#'
#'scaled.x.mat <- scale(X.mat)
#'x.sc.filtered <- scaled.x.mat[, attr(scaled.x.mat, "scaled:scale") != 0]
#'
#'w.vec <- rep(0, l=ncol(x.sc.filtered) + 1)
#'
#'LinearModelL1(x.sc.filtered,y.vec, penalty, opt.thresh, w.vec, step.size)
#'----------------------------------------------------------------------------
LinearModelL1 <- function(X.scaled.mat, y.vec, penalty, opt.thresh, initial.w.vec, step.size){


  if(!all(is.matrix(X.scaled.mat),is.numeric(X.scaled.mat))){
    stop("X.scaled.mat must be a numberic matrix!")
  }


  if (!all(is.vector(y.vec), is.numeric(y.vec),length(y.vec) == nrow(X.scaled.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.scaled.mat!")
  }


  if(!all(penalty >= 0, is.numeric(penalty))){
    stop("penalty must be an numeric greater or equal to 0!")
  }

  if(!all(is.numeric(opt.thresh), 0 < opt.thresh)){
    stop("opt.thresh must be a number greater 0!")
  }

  if(!step.size != 0){
    stop("n.hidden.units must be an interger greater or equal to 1!")
  }

  # get is.binary
  is.binary <- all(y.vec %in% c(0,1))

  int.X <- cbind(1, X.scaled.mat)
  if(is.binary){
    #obtain y.tilde
    y.tilde <- ifelse(y.vec==1,1,-1)
    pred.vec <- int.X %*% initial.w.vec
    prob.vec <- sigmoid(-pred.vec * y.tilde)
    grad.vec <- -t(int.X) %*% (y.tilde * prob.vec)



  }else{

    # browser()  # for debug
    grad.vec <- 2 * t(int.X %*% initial.w.vec - y.vec) %*% int.X
  }

  d.vec <- -grad.vec
  u.vec <- initial.w.vec + step.size*d.vec
  w <- c(u.vec[1], soft(u.vec[-1], step.size*penalty))
  return(w)
}




#'@method LinearModelL1penalties
#'@param X.mat (feature matrix, n x p)
#'@param y.vec (label vector, p x 1)
#'@param penalty.vec (numeric scalar >= 0)
#'@param step.size (positive scalar)
#'
#'@return optimal weight vector (p+1 elements, first element is the bias/intercept b)
#'
#' @export
#'
#' @examples
#' BINARY
#' ---------------
#'data(zip.train, package="ElemStatLearn")
#'all.y.vec <- zip.train[,1]
#'is.01 <- all.y.vec %in% c(0,1)
#'y.vec <- all.y.vec[is.01]
#'X.mat <- zip.train[is.01,-1]
#'
#'
#'step.size <- .01
#'set.seed(1)
#'penalty.vec <- as.numeric(sample(rep(1:10)))

#'LinearModelL1penalties(X.mat,y.vec, penalty.vec, step.size)
#'-----------
#'
#'REGRESSION
#'-----------
#'data(zip.train, package="ElemStatLearn")
#'y.vec <- zip.train[,1]
#'X.mat <- zip.train[,-1]
#'
#'
#'step.size <- .01
#'set.seed(1)
#'penalty.vec <- as.numeric(sample(rep(1:10)))

#'LinearModelL1penalties(X.mat,y.vec, penalty.vec, step.size)
#'----------------------------------------------------------------------------
LinearModelL1penalties <- function(X.mat, y.vec, penalty.vec, step.size){


  scaled.x.mat <- scale(X.mat)
  x.sc.filtered <- scaled.x.mat[, attr(scaled.x.mat, "scaled:scale") != 0]
  w.vec <- rep(0, l=ncol(x.sc.filtered) + 1)
  opt.thresh <- 1

  scaled.w.mat <- matrix(0,nrow = ncol(x.sc.filtered) + 1,ncol = length(penalty.vec))
  unscaled.w.mat <- scaled.w.mat

  for (index in 1:length(penalty.vec)){
    str(opt.thresh)
    w.vec <- LinearModelL1(x.sc.filtered,y.vec, penalty.vec[index], opt.thresh, w.vec, step.size)

    scaled.w.mat[,index] <- w.vec
    #browser()

    bias <- w.vec[1]
    # TODO: USE MEAN & STD TO SCALE VECTOR BACK UP

    opt.thresh <- min(abs(w.vec[which(w.vec != 0)]))


  }

  str(unscaled.w.mat)
  str(cbind(1,X.mat) %*% unscaled.w.mat) # prediction
}







# Helper Functions
# ========================
sigmoid <- function(z){
  1/(1+exp(-z))
}

soft <- function(x, lambda){
  sign(x) * positivepart(abs(x)-lambda)
}

positivepart <- function(x){
  ifelse(x>0,x,0)
}

