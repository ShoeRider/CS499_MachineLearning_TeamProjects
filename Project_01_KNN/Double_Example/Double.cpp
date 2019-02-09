#include <Rcpp.h>
using namespace Rcpp;

// Note need this line for any cpp code
// [[Rcpp::export]]


//Also when using RCPP, you cant use pointers in parameters for function calls
double double_me_cpp(double x) {
  // Doubles the value at the memory location pointed to by x
  x = x + x;
  return(x);
}
