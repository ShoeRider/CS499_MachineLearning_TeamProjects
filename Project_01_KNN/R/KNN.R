#Inline C++ implementation methods:
# note you need the: library(Rcpp)
# and can install Rcpp with the in R Command:
#   install.packages("Rcpp")
# Indlude the RCPP package with the line:
library(Rcpp)



sourceCpp("../src/KNN.cpp")
double_me_cpp(as.integer(5))
