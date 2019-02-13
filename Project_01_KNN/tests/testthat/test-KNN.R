#write tests for that R function, in tests/testthat/test-NNLearnCV.R.
# You should at least test that

# - (1) For valid inputs including a user-specified fold.vec your function
#           returns a list,
# - (2) the predict function returns a numeric vector of the expected size,
# - (3) for an invalid input, your function stops with an informative error message.


#Here is some example code to calculate both performance and validity

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




try(log(x))








# The following not all that relevant ....
# cool function that displays functions output in an error box
#- Function doesn't display in an error box if the return is not a message
message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}


#Example:
message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}

f <- function() 0
g <- function() message("Hi!")
g()
# Hi!
message2error(g())
message2error(f())
