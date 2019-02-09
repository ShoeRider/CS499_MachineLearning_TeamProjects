#include C .so methods with the command:
dyn.load("Double.so")
#this file was made from the Double.c file compiled from the CMD command:
#


#Running C from file methods
# Directly calling the C function implementation
.C("double_me", x = as.integer(5))

# double function implementation
double <- function(x){
  .C("double_me", x)
}
double(as.integer(5))







#Inline C++ implementation methods:
# note you need the: library(Rcpp)
# and can install Rcpp with the in R Command:
#   install.packages("Rcpp")
# Indlude the RCPP package with the line:
library(Rcpp)


cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')



#include CPP from file methods
library(Rcpp)
sourceCpp("Double.cpp")

double_me_cpp(as.integer(5))
