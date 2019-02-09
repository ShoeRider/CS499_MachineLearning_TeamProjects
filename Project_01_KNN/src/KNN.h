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

int knn(
   //Training Points
    double * training_inputs_ptr,
    double * training_lables_ptr,
    int NRow,int NCol,
    int MaxNeighbors,
    //Test Points
    double * testing_inputs_ptr,
    double * testing_Prediction_ptr
  );

double test(double x);
#endif // KNN_H
