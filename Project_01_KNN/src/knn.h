enum error_code
{
  NO_TRAIN_DATA_ERR=1,
  NO_TEST_DATA_ERR,
  KMAX_TOO_SMALL_ERR,
  KMAX_TOO_LARGE_ERR
};

int knn(
    const double * train_input_ptr, // n_observations x n_features
    const double * train_label_ptr, // n_observations
    const double * test_input_ptr, // n_features
    const int n_observations,
    const int n_features,
    const int max_neighbors,
    double * test_predict_ptr // max_neighbors
);

int NN1toKmaxPredict(const int n_train_observations,
                 const int n_test_observations,
                 const int n_features,
                 const int max_neighbors,
                 double * train_input_ptr, // n_train_observations x n_features
                 double * train_label_ptr, // n_train_observations
                 double * test_input_ptr, // n_test_observations x n_features
                 double * test_predict_ptr); // n_test_observations x max_neighbors