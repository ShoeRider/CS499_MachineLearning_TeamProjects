

/*Predict_1ToMAX_KNearestNeighbors
 Parameters:
(

),

*/
//Note: i am not sure if we are required to use (double pointers), or if we
//can use NumericVectors. I have an example of NumericVectors working in "SortVector_Eigen"
//But am unsure that is acceptable. Here is the version implemented in class:
int Predict_1ToMAX_KNearestNeighbors(
    double * training_inputs_ptr,
    double * training_lables_ptr,
    int NRow,int NCol,
    int MaxNeighbors,
    double * testing_inputs_ptr,
    double * testing_Prediction_ptr
)
{
  //TODO:

  //Take input parameters and map them with the Eigen objects
  Eigen::VectorXd Difference_Vector(NCol);
  Eigen::VectorXd Sorted_Index_Vector(NCol);

  Eigen::Map<Eigen::MatrixXd> TrainingInput(training_inputs_ptr,NRow,NCol);
  Eigen::Map<Eigen::MatrixXd> TrainingLabel(training_lables_ptr,NCol);

  Eigen::Map<Eigen::MatrixXd> TestingInput(testing_inputs_ptr,NRow,NCol);
  //Calculate Distance from TestPoint to Training data
  for(int i = 0; i < NRow; i++)
  {
    Difference_Vector(i) = (TrainingInput.row(i).transpose() - TestingInput).norm();
    Sorted_Index_Vector(i) = i;
  }


  //Sort the Distance Training Elements, so finding the nearest neighbors is quick
  //std::sort(Eigen_Vector.data(), Eigen_Vector.data()+Eigen_Vector.size());

  //Sort the Distance Training Elements from the calculated distance array,
  //  This uses the included Lambda Function
  std::sort(Sorted_Index_Vector.data(),
            Sorted_Index_Vector.data() + Sorted_Index_Vector.size(),
            [&Difference_Vector](int lhs, int rhs)
            {
              return Difference_Vector(lhs) < Difference_Vector(rhs);
            });

  double Label_Sum = 0.0;

  /*Loop from 1 to Max K NearestNeighbors


   */
  for(int i = 0; i < MaxNeighbors; i++)
  {
    //select element from accending Distance sorted list

    int NextNeighbor_VectorIndex = Sorted_Index_Vector(i);
    //accumulate TotalY

    Label_Sum += TrainingLabel[NextNeighbor_VectorIndex];
    //find the Y hat value for each element in the array = TotalY / i
    *testing_Prediction_ptr = Label_Sum / (i+1);
  }
  return 0;
}
