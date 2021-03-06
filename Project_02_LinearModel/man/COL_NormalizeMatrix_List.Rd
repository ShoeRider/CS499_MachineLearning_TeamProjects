% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/General.R
\name{COL_NormalizeMatrix_List}
\alias{COL_NormalizeMatrix_List}
\title{COLNormalizedMatrix}
\usage{
COL_NormalizeMatrix_List(Matrix)
}
\arguments{
\item{Matrix}{[X x Y]}
}
\value{
returns a NormalizedMatrix with the points in respect to the Matrix's Mean.

Uses the formula: ((Matrix - Matrix.mean)/Matrix.sd)
}
\description{
COLNormalizedMatrix
}
\examples{
Norm.Mat = NormalizeMatrix(as.matrix(c(1,2,3)))
print(Norm.Mat)
# Returns:
         [,1]
[1,] -1.224745
[2,]  0.000000
[3,]  1.224745
}
