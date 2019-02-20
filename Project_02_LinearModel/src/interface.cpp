#include <R.h> // for error
#include <R_ext/Rdynload.h> // for registration
#include "LinearModel.h"

//and Bias
void LMLogisticRegression_FindPrediction_interface(
    const double * train_input_ptr, // n_observations x n_features
    const double * train_label_ptr, // n_observations
    const double * test_input_ptr, // n_features
    const int n_observations,
    const int n_features,
    const double * scalars,
    const double bias,
    double * test_predict_ptr // max_neighbors
)
{
  LM_FindPrediction(
    train_input_ptr,
    train_label_ptr,
    test_input_ptr,
    n_observations,
    n_features,
    scalars,
    bias,
    test_predict_ptr);
}

R_CMethodDef cMethods[] = {
  {"LMLogisticRegression_FindPrediction_interface", (DL_FUNC) &LMLogisticRegression_FindPrediction_interface,8},
  {NULL, NULL, 0}
};

extern "C" {
  void R_init_LinearModel(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}
