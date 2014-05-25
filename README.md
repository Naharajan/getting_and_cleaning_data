Course Project - Getting and Cleaning data
========================================================

1) Opening the test and the train data:
   The training data and the test data (X_test.txt) were first loaded on to the R workspace using the read.table command. The corresponding activity files and the subject files are also loaded.
   
2) Descriptive activity names assigned to their labels:
   The six activities as described were then assigned and appended to the training and test data correspondign to their activity labels. 
   
3) Getting the feature names: 
   The features names from features.txt were extracted and then they were assigned as column names for the train and the test data. 
   
4) Merging of train and test data:
  The train ans the test data were then merged using the rbind command.

5) Extracting only the measurements on the mean and standard deviation for each measurement:
   The mean and standard deviation for each measurement were then extracted using the grep command. 
   
6) Calculating the average of each variable for each activity and each subject:
   Finally the avergaes for each of those variables were calculates using the ddply command from the plyr package.