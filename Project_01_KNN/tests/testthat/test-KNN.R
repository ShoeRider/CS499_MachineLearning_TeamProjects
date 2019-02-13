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



#2 points extra credit if, in your R package, you write a test that makes sure
# your C++ nearest neighbors code computes the same predictions as a nearest
# neighbor prediction computed in R code.

#2 points extra credit if, in your Rmd report, you compute the test loss
# matrices by writing a loop over all five data sets. (rather than copying/
# repeating the same CV code for each data set) Hint: use store the data sets in
# a named list.

#2 points extra credit if, in your Rmd report, you use LaTeX code/MathJax to
# type the equations for the nearest neighbor prediction function fD,k(x) and
# the optimal number of neighbors \hat k (as estimated via minimizing the mean
# validation loss).

#2 points if, in your GitHub repo, you setup Travis-CI to check your R package,
# and have a green badge that indicates a build that passes checks. See blog and
# docs.


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
