#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double double_me_cpp(double x) {
  // Doubles the value at the memory location pointed to by x
  x = x + x;
  return(x);
}
