int LM_FindPrediction(
    const double * train_input_ptr, // n_observations x n_features
    const double * train_label_ptr, // n_observations
    const double * test_input_ptr, // n_features
    const int n_observations,
    const int n_features,
    const double * scalars,
    const double bias,
    double * test_predict_ptr // max_neighbors
);
