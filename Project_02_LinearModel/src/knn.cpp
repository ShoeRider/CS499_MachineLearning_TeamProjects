#include <Eigen/Dense>
#include "knn.h"

int knn(
    const double * train_input_ptr, // n_observations x n_features
    const double * train_label_ptr, // n_observations
    const double * test_input_ptr, // n_features
    const int n_observations,
    const int n_features,
    const int max_neighbors,
    double * test_predict_ptr // max_neighbors
)
{
  // error checking
  
  // validate positive train data dimensions
  if (n_observations < 1)
  {
    return NO_TRAIN_DATA_ERR;
  }
  
  // validate kmax is positive
  if (max_neighbors < 1)
  {
    return KMAX_TOO_SMALL_ERR;
  }
  
  // validate kmax < nrow
  if (max_neighbors >= n_observations)
  {
    return KMAX_TOO_LARGE_ERR;
  }
  
  Eigen::VectorXd distance_vec(n_observations);
  Eigen::Map<Eigen::MatrixXd> train_inputs_mat(
      (double *) train_input_ptr, n_observations, n_features);
  Eigen::Map<Eigen::VectorXd> test_input_vec(
      (double *) test_input_ptr, n_features);
  Eigen::VectorXi sorted_index_vec(n_observations);
  
  for (int i = 0; i < n_observations; i++)
  {
    distance_vec(i) = (
      train_inputs_mat.row(i).transpose() - test_input_vec
    ).lpNorm<1>(); // manhattan distance?
    sorted_index_vec(i) = i;
  }
  
  std::sort(
    sorted_index_vec.data(),
    sorted_index_vec.data() + sorted_index_vec.size(),
    [&distance_vec](int left, int right) {
      return distance_vec(left) < distance_vec(right);
    }
  );
  
  int row; int neighbors; double total_labels = 0.0;
  
  for (int k = 0; k < max_neighbors; k++)
  {
    row = sorted_index_vec(k);
    neighbors = k + 1;
    total_labels += train_label_ptr[row];
    test_predict_ptr[k] = total_labels/neighbors;
  }
  
  return 0;
}


int NN1toKmaxPredict(const int n_train_observations,
                 const int n_test_observations,
                 const int n_features,
                 const int max_neighbors,
                 double * train_input_ptr, // n_train_observations x n_features
                 double * train_label_ptr, // n_train_observations
                 double * test_input_ptr, // n_test_observations x n_features
                 double * test_predict_ptr) // n_test_observations x max_neighbors
{
  
  // error checking
  
  // validate positive train data dimensions
  if (n_train_observations < 1)
  {
    return NO_TRAIN_DATA_ERR;
  }
  
  // validate positive train data dimensions
  if (n_test_observations < 1)
  {
    return NO_TEST_DATA_ERR;
  }
  
  // validate kmax is positive
  if (max_neighbors < 1)
  {
    return KMAX_TOO_SMALL_ERR;
  }
  
  // validate kmax < nrow
  if (max_neighbors >= n_train_observations)
  {
    return KMAX_TOO_LARGE_ERR;
  }
  
  Eigen::Map<Eigen::MatrixXd> test_input_mat(
      test_input_ptr,n_features, n_test_observations);
  Eigen::Map<Eigen::MatrixXd> test_predict_mat(
    test_predict_ptr, max_neighbors, n_test_observations);
  
  for (int i = 0; i < n_test_observations; i++)
  {
    knn(train_input_ptr, train_label_ptr,
        test_input_mat.col(i).data(),
        n_train_observations, n_features, max_neighbors,
        test_predict_mat.col(i).data());
  }
  
  return 0;
}
        
  
  
  
  
  
  
  
  
  