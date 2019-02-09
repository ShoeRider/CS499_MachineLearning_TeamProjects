Predict_1ToMAX_KNearestNeighbors <-.C("Predict_1ToMAX_KNearestNeighbors",
  as.double(),
  )

dyn.load("../doubler.so")
.C("double_me", x = as.integer(5))
