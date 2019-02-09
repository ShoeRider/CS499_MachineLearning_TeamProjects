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


double knn(
    double SortedTraining_inputs_ptr,
    double SortedTraining_lables_ptr,
    int NRow,
    int NCol,
    int MaxNeighbors,
    double testing_inputs_ptr,
    double testing_Prediction_ptr
  );



#endif // KNN_H
