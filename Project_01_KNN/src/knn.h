int knn(
    const double *train_input_ptr, // n_observations x n_features
    const double *train_label_ptr, // n_observations
    const double *test_input_ptr, // n_features
    const int n_observations_ptr,
    const int n_features_ptr,
    const int max_neighbors_ptr,
    double *test_prediction_ptr // max neighbors
);