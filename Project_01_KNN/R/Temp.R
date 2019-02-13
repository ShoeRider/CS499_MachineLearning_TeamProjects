library(rbenchmark)
library(Rcpp)

#include local files
sourceCpp("../../src/KNN.cpp")


set.seed(123)
z <- rnorm(100000)
x <- rnorm(100)

# check that SortVector_Eigen is the same as sort
stopifnot(all.equal(SortVector_Eigen(x), sort(x)))

# benchmark SortVector_Eigen and sort
benchmark(SortVector_Eigen(z), sort(z), order="relative")[,1:4]
