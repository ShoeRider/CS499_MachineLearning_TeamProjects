#include "knn.h"
#include <R.h> // for error
#include <R_ext/Rdynload.h> // for registration

void knn_interface(
    const double * train_input_ptr, // n_observations x n_features
    const double * train_label_ptr, // n_observations
    const double * test_input_ptr, // n_features
    const int * n_observations_ptr,
    const int * n_features_ptr,
    const int * max_neighbors_ptr,
    double * test_predict_ptr // max_neighbors
)
{
int status = knn(train_input_ptr, train_label_ptr,
                   test_input_ptr, *n_observations_ptr,
                   *n_features_ptr, *max_neighbors_ptr,
                   test_predict_ptr);

  // error messages
  if (status != 0)
  {
    if (status == NO_TRAIN_DATA_ERR)
    {
      error("no train data");
    }
    
    else if (status == KMAX_TOO_SMALL_ERR)
    {
      error("too many neighbors");
    }
    
    else if (status == KMAX_TOO_LARGE_ERR)
    {
      error("too few neighbors");
    }
    
    else
    {
      error("unknown non-zero exit status from knn", status);
    }
  }
}

void NN1toKmaxPredict_interface(
    const int * n_train_observations_ptr,
    const int * n_test_observations_ptr,
    const int * n_features_ptr,
    const int * max_neighbors_ptr,
    double * train_input_ptr, // n_train_observations x n_features
    double * train_label_ptr, // n_train_observations
    double * test_input_ptr, // n_test_observations x n_features
    double * test_predict_ptr // n_test_observations x max_neighbors
  )
{
  int status = NN1toKmaxPredict(*n_train_observations_ptr,
                                *n_test_observations_ptr,
                                *n_features_ptr,
                                *max_neighbors_ptr,
                                train_input_ptr,
                                train_label_ptr,
                                test_input_ptr,
                                test_predict_ptr);
  
  // error messages
  if (status != 0)
  {
    if (status == NO_TRAIN_DATA_ERR)
    {
      error("no train data");
    }
    
    else if (status == NO_TEST_DATA_ERR)
    {
      error("no test data");
    }
    
    else if (status == KMAX_TOO_SMALL_ERR)
    {
      error("too many neighbors");
    }
    
    else if (status == KMAX_TOO_LARGE_ERR)
    {
      error("too few neighbors");
    }
    
    else
    {
      error("unknown non-zero exit status from knn", status);
    }
  }
}

R_CMethodDef cMethods[] = {
  {"knn_interface", (DL_FUNC) &knn_interface, 7},
  {"NN1toKmaxPredict_interface", (DL_FUNC) &NN1toKmaxPredict_interface, 8},
  {NULL, NULL, 0}
};

extern "C" {
  void R_init_NearestNeighbors(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}