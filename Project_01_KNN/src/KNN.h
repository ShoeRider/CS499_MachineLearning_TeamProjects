#ifndef KNN_H
#define KNN_H

#include <iostream>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include "Eigen/Dense"// "" will install this Package

//for extra credit: the OpenMP Package
#include "omp.h" //"sudo apt-get install libomp-dev" will install this Package

/*NN1toKmaxPredict takes a training data set and an entire test matrix,
 then computes a matrix of k-nearest neighbor predictions,
 for k=1 to max_neighbors, and for every test observation.
  Parameters:
  (
  n_train_observations, n_test_observations, n_features.
  maximum number of neighbors: max_neighbors.
  a matrix training data inputs (n_train_observations x n_features).
  a vector of training data outputs (n_train_observations).
  a test input matrix (n_test_observations x n_features).
  a matrix of predictions for the test data (n_test_observations x max_neighbors),
      which is where you need to store the result.

  ),
  */
  double NN1toKmaxPredict(
    double Training_inputs_ptr,
    double Training_lables_ptr,
    int N_TrainingObservations,
    int N_Features,
    int NRow,
    int NCol,
    int MaxNeighbors,
    double testing_inputs_ptr,
    double testing_Prediction_ptr,
    int N_TestObservations
  );




/*Takes:
    (

    ),
    Modifies the Vector at: testing_Prediction_ptr,
      selecting a classification for a single point
    Note: this function might be able to be made faster by segmenting points into a sorted 'bucket',
      By sorting once, we might see gains with Larger lists of training data...
  */
int Predict_1ToMAX_KNearestNeighbors(
     //Training Points
      double * training_inputs_ptr,
      double * training_lables_ptr,
      int NRow,int NCol,
      int MaxNeighbors,
      //Test Points
      double * testing_inputs_ptr,
      double * testing_Prediction_ptr
    );


#define ERROR_TOO_MANY_NEIGHBORS 1
#define ERROR_TOO_FEW_NEIGHBORS 2
#define ERROR_NO_TRAIN_DATA 3
#define ERROR_NO_TEST_DATA 4

#endif // KNN_H
