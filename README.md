# Spatial classification through probabilistic label relaxation on satellite image

Input image: ilk-3b-1024.tif

Training set: ilk-tr-x.txt

Test set: ilk-te-xy.txt

Ground truth info: ilk-te-xy-gt.txt

## This code accomplishes the following: 

1) Uses training data points in centered in classes to increase the size of training data by adding the queen's neighbors to the training set with same class label.

2) Creates a decision tree model on the data.

3) Writes classified image using the model created.

4) Performs probabilistic label relaxation to reduce noise in the output hence performing "spatial classification".

Dependencies: rgdal, rpart

To run this code: source("Probabilistic label relaxation.R")

Contact: tnybny@gmail.com
