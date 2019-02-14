#include "knn.h"
#include <R.h>
#include <R_ext/Rdynload.h>

void knn_interface(
    const double *train_input_ptr, // n_observations x n_features
    const double *train_label_ptr, // n_observations
    const double *test_input_ptr, // n_features
    const int *n_observations_ptr,
    const int *n_features_ptr,
    const int *max_neighbors_ptr,
    double *test_prediction_ptr // max neighbors
)
{
  int status = knn(train_input_ptr,train_label_ptr,test_input_ptr,
                   *n_observations_ptr, *n_features_ptr, *max_neighbors_ptr,
                   test_prediction_ptr);
  
  if( status != 0 )
  {
    error("Non-zero Exit Status from KNN");
  }
}

R_CMethodDef cMethods[]= {
  {"knn_interface",(DL_FUNC) &knn_interface, 7 },
  { NULL, NULL, 0 }
};

extern "C" {
  void R_init_nearestNeighborsAlg(DllInfo *info){
    R_registerRoutines( info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols( info, FALSE);
  }
}
