//Implement KNN
//Implement KNN_1ToMax

/*Takes:
  (
  
  ),
  Modifies the Vector at: testing_Prediction_ptr, 
    selecting a classification for a single point
  Note: this function might be able to be made faster by segmenting points into a sorted 'bucket',
    By sorting once, we might see gains with Larger lists of training data...
*/
int Predict_1ToMAX_KNearestNeighbors
  (
   //Training Points
    double * training_inputs_ptr,
    double * training_Lables,
    int NRow,int NCol, 
    int MaxNeighbors,
    //Test Points
    double * testing_inputs_ptr,
    double * testing_Prediction_ptr
  )
{
  //TODO:
  //Take input parameters and map them with the Eigen objects
  //
  //Sort Training Elements, so finding the nearest neigboors is quick
  //
  
  
  //Loop from 1 to Max K NearestNeighbors
  //Could use OpenMP to Parallelize this component
  for(int i=0, i < MaxNeighbors, i++)
  {
    //select element from accending Distance sorted list
    
    //accumulate TotalY
    //find the Y hat value for each element in the array = TotalY / i
  }
  return 0;
}

/*Takes:
  (
  
  ),
  Modifies the Vector at: testing_Prediction_ptr, 
    selecting a classification for a single point
  Note: this function might be able to be made faster by segmenting points into a sorted 'bucket',
    By sorting once, we might see gains with Larger lists of training data...
*/
int PredictTestArray_1ToMAX_KNearestNeighbors
  (
   //Training Points
    double * training_inputs_ptr,
    double * training_Lables,
    int NRow,int NCol, 
    int MaxNeighbors,
    //Test Points
    double * testing_inputs_ptr,
    double * testing_Prediction_ptr,
    int NTest_Row,int NTest_Col
  )
 {
	//TODO:
    //Take input parameters and map them with the Eigen objects
    //Create a Sorted List for an individual element
	//for loop over the entire testing_Prediction_ptr Array
	return 0;
 }
 
 
 //Use Sorting Technique to sort elements 
int SortTrainingData
  (
   //Training Points
    double * training_inputs_ptr,
    double * training_Lables,
    int NRow,int NCol
  )
  {
	 //Could use InsertSort or Merge Sort(Parallelizeable) to sort the training data in an attempt to save time.
	return 0; //no issues encountered
  }
 
 
 
 
 //The way i see the KNN Problem is to find an array of predictions from a KNN Problem, 
 // and find the instance of K, where with the lowest test error. 

 
 
 
 
 
 
 
 
 
 
  
