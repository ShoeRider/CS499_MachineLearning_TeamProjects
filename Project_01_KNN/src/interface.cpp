#include "KNN.h"
#include <R.h>

int knn_interface(
  //Training Points
   const double * training_inputs_ptr,
   const double * training_lables_ptr,
   const int NRow,const int NCol, const int MaxNeighbors,
   //Test Points
   double * testing_inputs_ptr,
   double * testing_Prediction_ptr
 )
{
  int status = knn(
    //Training Points
     training_inputs_ptr,
     training_lables_ptr,
     NRow,NCol,MaxNeighbors,
     //Test Points
     testing_inputs_ptr,
     testing_Prediction_ptr
   );
if (status != 0)
{
  error("non-zero exit status from knn");
}
}
