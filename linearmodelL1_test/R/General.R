#' NormalizeMatrix
#'
#' @param Matrix [X x Y]
#'
#' @return returns a NormalizedMatrix with the points in respect to the Matrix's Mean.
#'
#' Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
#'
#' @export
#'
#' @examples
#' Norm.Mat = NormalizeMatrix(as.matrix(c(1,2,3)))
#' print(Norm.Mat)
#' # Returns:
#'          [,1]
#'[1,] -1.224745
#'[2,]  0.000000
#'[3,]  1.224745
NormalizeMatrix<-function(Matrix)
{
  # vector of mean column values
  mean <- mean(Matrix)
  # sum all of all columns for each row
  sum = 0
  for( row in 1:nrow(Matrix))
  {
    sum = sum + sum((Matrix[row,] - mean)^2)
  }

  # get sd from the calculated sum and number of observations
  sd = sqrt(sum / length(Matrix))
  # return the new matrix
  return((Matrix - mean)/sd)
}



#' NormalizeMatrix_List
#'
#' @param Matrix [X x Y]
#'
#' @return Same as NormalizeMatrix, but returns contents within a list, so the standard deviation can be used later
#'
#' Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
#'
#' @export
#'
#' @examples
#' Norm_List = NormalizeMatrix_List(as.matrix(c(1,2,3)))
#' print(Norm_List)
#' # Returns:
#' $NormalizedMatrix
#' [,1]
#' [1,] -1.224745
#' [2,]  0.000000
#' [3,]  1.224745
#'
#' $sd
#' [1] 0.8164966
NormalizeMatrix_List<-function(Matrix)
{
  # vector of mean column values
  mean <- mean(Matrix)
  # sum all of all columns for each row
  sum = 0
  for( row in 1:nrow(Matrix))
  {
    sum = sum + sum((Matrix[row,] - mean)^2)
  }

  # get sd from the calculated sum and number of observations
  sd = sqrt(sum / length(Matrix))
  # return the new matrix
  ReturnList <-
    list(
      NormalizedMatrix = ((Matrix - mean)/sd),
      sd = sd
    )
}

#' DeNormalizeMatrix_List
#'
#' @param Matrix [X x Y]
#'
#' @return Same as NormalizeMatrix, but returns contents within a list, so the standard deviation can be used later
#'
#' Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
#'
#' @export
#'
#' @examples
#' Norm_List = NormalizeMatrix_List(as.matrix(c(1,2,3)))
#' print(Norm_List)
#' # Returns:
#' $NormalizedMatrix
#' [,1]
#' [1,] -1.224745
#' [2,]  0.000000
#' [3,]  1.224745
#'
#' $sd
#' [1] 0.8164966
DeNormalizeMatrix_List<-function(Matrix,List)
{
#ToDo ....
}


#'Random_Folds
#'   Simple Function that creates a list of integers that indicates the fold Number each element is apart of.
#' I included error checking to prevent any incorrect parameters.
#'@param Size is the length of the array to be produced []
#'@param Folds the number of numbers to be produced with the range(1,Folds)
#'
#'@return list of [Size] length with random integers with the range(1,Folds)
#'
#'@examples
#'  ~~ Example: 1 ~~
#' Folds =  Random_Folds(10,2)
#' print(Folds)
#' Returns:
#' [1] 1 1 2 2 2 1 1 1 2 1
#'
#'  ~~ Example: 2 ~~
#'  Folds =  Random_Folds(10,5)
#'  print(Folds)
#'  [1] 4 3 1 5 5 1 5 3 1 5
#'
Random_Folds <- function(Size,Folds)
{
  try(if(Size < 0) stop("Invalid Size Value: cannot preform random Folds when (Size < 0) !!"))
  try(if(Folds < 0) stop("Invalid Folds Value: cannot preform random Folds when (Size < 0) !!"))
  as.double(sample(0:(Folds),Size,replace=T))
}


#'Find.L1ErrorMean
#'
#' Find.L1ErrorMean, helps you find the L1 mean of your Y.Hat Results from a MachineLearning Algorithm.
#'
#'@param YHat.Labels, is the classification vector [I,n] you wish to test, where I is the number of Test causes, and n is the number of classifications
#'@param Y.Lables, is the true classification vector [n] you wish to test, where n is the number of classifications
#'
#'@param BinaryClassification an integer value determining if the Data set is a BinaryClassification, with only 0 and 1's as the Labels for each data set
#'
#'@return The Mean error of each the Column [n], which is an average of each
#'
#'
#'@examples
#'
Find.L1ErrorMean<-function(YHat.Labels,Y.Lables,BinaryClassification)
{
  #print("finding L2Error")

  YHat.Lables  = data.matrix(YHat.Labels)
  Y.Lables     = data.matrix(Y.Lables)
  #print("Yhat.Dim")
  #print(dim(YHat.Lables))
  #print(dim(Y.Lables))

  if(BinaryClassification)
  {
    # ifelse(YHat.Labels>0.5,1,0); Checks if element is greater than .5 and rounds up if so, if not rounds down
    # != as.integer(TestingLables); then compaire the output to the Y.Labels
    Error_Vector = ifelse(YHat.Labels>0.5,1,0) != as.integer(Y.Lables)
    #Error_Vector = ((YHat.Labels) - as.integer( TestingLables))**2
  }
  else
  {
    Error_Vector = abs((YHat.Labels) - as.double(Y.Lables))
  }

  L2Error.Matrix = (as.matrix(colMeans(t(Error_Vector))))
}


#'Find.L2ErrorMean
#'
#' Find.L2ErrorMean, helps you find the L2 mean of your Y.Hat Results from a MachineLearning Algorithm.
#'
#'@param YHat.Labels, is the classification vector [I,n] you wish to test, where I is the number of Test causes, and n is the number of classifications
#'@param Y.Lables, is the true classification vector [n] you wish to test, where n is the number of classifications
#'
#'@param BinaryClassification an integer value determining if the Data set is a BinaryClassification, with only 0 and 1's as the Labels for each data set
#'
#'@return The Mean error of each the Column [n], which is an average of each
#'
#'
#'@examples
#'
Find.L2ErrorMean<-function(YHat.Labels,Y.Lables,BinaryClassification)
{
  #print("finding L2Error")

  YHat.Lables  = data.matrix(YHat.Labels)
  Y.Lables     = data.matrix(Y.Lables)
  #print("Yhat.Dim")
  #print(dim(YHat.Lables))
  #print(dim(Y.Lables))

  if(BinaryClassification)
  {
    # ifelse(YHat.Labels>0.5,1,0); Checks if element is greater than .5 and rounds up if so, if not rounds down
    # != as.integer(TestingLables); then compaire the output to the Y.Labels
    Error_Vector = ifelse(YHat.Labels>0.5,1,0) != as.integer(Y.Lables)
    #Error_Vector = ((YHat.Labels) - as.integer( TestingLables))**2
  }
  else
  {
    Error_Vector = ((YHat.Labels) - as.double(Y.Lables))**2
  }

  L2Error.Matrix = (as.matrix(colMeans(t(Error_Vector))))
}








#' COLNormalizedMatrix
#'
#' @param Matrix [X x Y]
#'
#' @return returns a NormalizedMatrix with the points in respect to the Matrix's Mean.
#'
#' Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
#'
#' @export
#'
#' @examples
#' Norm.Mat = NormalizeMatrix(as.matrix(c(1,2,3)))
#' print(Norm.Mat)
#' # Returns:
#'          [,1]
#'[1,] -1.224745
#'[2,]  0.000000
#'[3,]  1.224745
COL_NormalizeMatrix_List<-function(Matrix)
{
  scaled.mat = matrix(nrow = nrow(Matrix),ncol = ncol(Matrix))
  col.means = as.matrix(rep(0,NCOL(Matrix)),dim=c(0,NCOL(Matrix)))
  col.sd = as.matrix(rep(0,NCOL(Matrix)),dim=c(0,NCOL(Matrix)))

  for(col in 1:ncol(Matrix))
  {
    scaled.mat[,col] = (Matrix[,col] - colMeans(Matrix)[col])/sd(Matrix[,col])
    col.means[col] = colMeans(Matrix)[col]
    col.sd[col] =  sd(Matrix[,col])
  }

  ReturnList <-
    list(
      Matrix = as.matrix(scaled.mat),
      col.means = col.means,
      col.sd = col.sd
    )
}

COL_DeNormalizeMatrix_List<-function(COL_NormalizeMatrix_List)
{
  Norm.Mat = COL_NormalizeMatrix_List$Matrix
  DeNorm.mat = matrix(nrow = nrow(Norm.Mat),ncol = ncol(Norm.Mat))
  #as.matrix(rep(0,NCOL(Matrix)),dim=c(0,NCOL(Matrix)))

  col.sd =as.matrix(COL_NormalizeMatrix_List$sd)
  col.means = as.matrix(COL_NormalizeMatrix_List$col.means)

  print(ncol(Norm.Mat))
  for(col in 1:ncol(Norm.Mat))
  {
    DeNorm.mat[,col] = (Norm.Mat[,col]+col.means[col])#*col.sd[col]

    #print("VS")
    #print(dim(as.matrix(Norm.Mat[,col]+col.means[col])))
    #print(dim(col.sd))
  }

  return (as.matrix(DeNorm.mat))
}




#' NormalizeMatrix
#'
#' @param Matrix [X x Y]
#'
#' @return returns a NormalizedMatrix with the points in respect to the Matrix's Mean.
#'
#' Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
#'
#' @export
#'
#' @examples
#' Norm.Mat = NormalizeMatrix(as.matrix(c(1,2,3)))
#' print(Norm.Mat)
#' # Returns:
#'          [,1]
#'[1,] -1.224745
#'[2,]  0.000000
#'[3,]  1.224745
NormalizeMatrix<-function(Matrix)
{
  # vector of mean column values
  mean <- mean(Matrix)
  # sum all of all columns for each row
  sum = 0
  for( row in 1:nrow(Matrix))
  {
    sum = sum + sum((Matrix[row,] - mean)^2)
  }

  # get sd from the calculated sum and number of observations
  sd = sqrt(sum / length(Matrix))
  # return the new matrix
  return((Matrix - mean)/sd)

}




#' NormalizeVector
#'
#' @param Vector [X]
#'
#' @return takes the absolute sum making the total distance, and divides by the length
#'
#' Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
#'
#' @export
#'
#' @examples
#' Norm_List = NormalizeMatrix_List(as.matrix(c(1,2,3)))
#' print(Norm_List)
#' # Returns:
#' $NormalizedMatrix
#' [,1]
#' [1,] -1.224745
#' [2,]  0.000000
#' [3,]  1.224745
#'
#' $sd
#' [1] 0.8164966
NormalizeVector<-function(Vector)
{
  #return<- Vector /sum(abs(Vector))
  return<- Vector /sum(Vector)
}
