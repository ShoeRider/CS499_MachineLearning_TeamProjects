library(rbenchmark)
library(Rcpp)

#include local files
sourceCpp("../src/KNN.cpp")


set.seed(123)
z <- rnorm(100000)
x <- rnorm(100)

# check that SortVector is the same as sort
stopifnot(all.equal(SortVector(x), sort(x)))


# benchmark SortVector and sort
benchmark(SortVector(z), sort(z), order="relative")[,1:4]
