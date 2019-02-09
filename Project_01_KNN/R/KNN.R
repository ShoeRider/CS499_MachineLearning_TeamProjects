library(Rcpp)
library(rbenchmark)

#include local files
sourceCpp("../src/KNN.cpp")



x <- rnorm(5)
SortVector(x)



#Defined R's KNN implementation

knn_R <- function(TrainingInput, TrainingLabel, Neighbors, TestInput, TestPrediction)
{

}
