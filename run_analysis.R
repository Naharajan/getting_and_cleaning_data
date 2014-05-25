library(plyr)

###Open the train and test data sets
#setwd("/Users/lakshman/Documents/Coursera/getting_cleaning_data/")
test_data<-read.table("UCI HAR Dataset/test/X_test.txt")
test_data_activity<-read.table("UCI HAR Dataset/test/y_test.txt")
test_data_subject<-read.table("UCI HAR Dataset/test/subject_test.txt")
train_data<-read.table("UCI HAR Dataset/train/X_train.txt")
train_data_activity<-read.table("UCI HAR Dataset/train/y_train.txt")
train_data_subject<-read.table("UCI HAR Dataset/train/subject_train.txt")

###Using the descriptive activity names to name the activities in the data set
test_data_activity_transformed<-transform(test_data_activity, Activity=ifelse(V1==1, "WALKING", ifelse(V1==2, "WALKING_UPSTAIRS",ifelse(V1==3, "WALKING_DOWNSTAIRS",ifelse(V1==4, "SITTING",ifelse(V1==5, "STANDING",ifelse(V1==6, "LAYING", "NO")))))))
train_data_activity_transformed<-transform(train_data_activity, Activity=ifelse(V1==1, "WALKING", ifelse(V1==2, "WALKING_UPSTAIRS",ifelse(V1==3, "WALKING_DOWNSTAIRS",ifelse(V1==4, "SITTING",ifelse(V1==5, "STANDING",ifelse(V1==6, "LAYING", "NO")))))))

####Getting the feature names 
features<-read.table("UCI HAR Dataset/features.txt")
variables<-features$V2

colnames(test_data)<-variables
colnames(train_data)<-variables
test_data$activity<-test_data_activity_transformed$Activity
test_data$subject<-test_data_subject$V1

train_data$activity<-train_data_activity_transformed$Activity
train_data$subject<-train_data_subject$V1

###Merging the training and the test sets to create one data set.
train_and_test<-rbind(train_data, test_data)

###Extracting only the measurements on the mean and standard deviation for each measurement
sd<-train_and_test[,grep("*std()*", names(train_and_test), value=TRUE)]
mean_1<-train_and_test[,grep("*mean()*", names(train_and_test), value=TRUE)]
mean<-mean_1[,!grepl("*meanFreq()*", names(mean_1))]
subject_activity<-train_and_test[,c(563, 562)]
tidy_dataset<-cbind(subject_activity,mean,sd)

####Calculating the average of each variable for each activity and each subject
names(tidy_dataset)<-gsub("-","_", names(tidy_dataset))
names(tidy_dataset)<-gsub("[()]","", names(tidy_dataset))
tidy_dataset_1 <-ddply(tidy_dataset, .(subject, activity), summarize,tBodyAcc_mean_X=mean(tBodyAcc_mean_X),tBodyAcc_mean_Y=mean(tBodyAcc_mean_Y),tBodyAcc_mean_Z=mean(tBodyAcc_mean_Z),
                     tGravityAcc_mean_X=mean(tGravityAcc_mean_X),     
                     tGravityAcc_mean_Y=mean(tGravityAcc_mean_Y),     
                     tGravityAcc_mean_Z=mean(tGravityAcc_mean_Z), 
                     tBodyAccJerk_mean_X =mean(tBodyAccJerk_mean_X),   
                     tBodyAccJerk_mean_Y=mean(tBodyAccJerk_mean_Y),   
                     tBodyAccJerk_mean_Z=mean(tBodyAccJerk_mean_Z),      
                     tBodyGyro_mean_X=mean(tBodyGyro_mean_X),     
                     tBodyGyro_mean_Y=mean(tBodyGyro_mean_Y),
                     tBodyGyro_mean_Z=mean(tBodyGyro_mean_Z),       
                     tBodyGyroJerk_mean_X=mean(tBodyGyroJerk_mean_X),
                     tBodyGyroJerk_mean_Y=mean(tBodyGyroJerk_mean_Y),   
                     tBodyGyroJerk_mean_Z=mean(tBodyGyroJerk_mean_Z),      
                     tBodyAccMag_mean=mean(tBodyAccMag_mean),      
                     tGravityAccMag_mean=mean(tGravityAccMag_mean),     
                     tBodyAccJerkMag_mean=mean(tBodyAccJerkMag_mean),   
                     tBodyGyroMag_mean=mean(tBodyGyroMag_mean),        
                     tBodyGyroJerkMag_mean=mean(tBodyGyroJerkMag_mean),   
                     fBodyAcc_mean_X=mean(fBodyAcc_mean_X),        
                     fBodyAcc_mean_Y=mean(fBodyAcc_mean_Y),       
                     fBodyAcc_mean_Z=mean(fBodyAcc_mean_Z),        
                     fBodyAccJerk_mean_X=mean(fBodyAccJerk_mean_X),    
                     fBodyAccJerk_mean_Y=mean(fBodyAccJerk_mean_Y),   
                     fBodyAccJerk_mean_Z=mean(fBodyAccJerk_mean_Z),    
                     fBodyGyro_mean_X=mean(fBodyGyro_mean_X),         
                     fBodyGyro_mean_Y=mean(fBodyGyro_mean_Y),       
                     fBodyGyro_mean_Z=mean(fBodyGyro_mean_Z),         
                     fBodyAccMag_mean=mean(fBodyAccMag_mean),    
                     fBodyBodyAccJerkMag_mean=mean(fBodyBodyAccJerkMag_mean),  
                     fBodyBodyGyroMag_mean=mean(fBodyBodyGyroMag_mean), 
                     fBodyBodyGyroJerkMag_mean=mean(fBodyBodyGyroJerkMag_mean),
                     tBodyAcc_std_X=mean(tBodyAcc_std_X),       
                     tBodyAcc_std_Y=mean(tBodyAcc_std_Y),
                     tBodyAcc_std_Z=mean(tBodyAcc_std_Z),        
                     tGravityAcc_std_X=mean(tGravityAcc_std_X),      
                     tGravityAcc_std_Y=mean(tGravityAcc_std_Y),     
                     tGravityAcc_std_Z=mean(tGravityAcc_std_Z),        
                     tBodyAccJerk_std_X=mean(tBodyAccJerk_std_X),      
                     tBodyAccJerk_std_Y=mean(tBodyAccJerk_std_Y),
                     tBodyAccJerk_std_Z=mean(tBodyAccJerk_std_Z),       
                     tBodyGyro_std_X=mean(tBodyGyro_std_X),
                     tBodyGyro_std_Y=mean(tBodyGyro_std_Y),        
                     tBodyGyro_std_Z=mean(tBodyGyro_std_Z),
                     tBodyGyroJerk_std_X=mean(tBodyGyroJerk_std_X),   
                     tBodyGyroJerk_std_Y=mean(tBodyGyroJerk_std_Y),
                     tBodyGyroJerk_std_Z=mean(tBodyGyroJerk_std_Z),   
                     tBodyAccMag_std=mean(tBodyAccMag_std),         
                     tGravityAccMag_std=mean(tGravityAccMag_std),     
                     tBodyAccJerkMag_std=mean(tBodyAccJerkMag_std),      
                     tBodyGyroMag_std=mean(tBodyGyroMag_std),      
                     tBodyGyroJerkMag_std=mean(tBodyGyroJerkMag_std),fBodyAcc_std_X=mean(fBodyAcc_std_X),fBodyAcc_std_Y=mean(fBodyAcc_std_Y),fBodyAcc_std_Z=mean(fBodyAcc_std_Z),fBodyAccJerk_std_X=mean(fBodyAccJerk_std_X),fBodyAccJerk_std_Y=mean(fBodyAccJerk_std_Y),fBodyAccJerk_std_Z=mean(fBodyAccJerk_std_Z),fBodyGyro_std_X=mean(fBodyGyro_std_X),fBodyGyro_std_Y=mean(fBodyGyro_std_Y),fBodyGyro_std_Z=mean(fBodyGyro_std_Z),fBodyAccMag_std=mean(fBodyAccMag_std),fBodyBodyAccJerkMag_std=mean(fBodyBodyAccJerkMag_std),fBodyBodyGyroMag_std=mean(fBodyBodyGyroMag_std),fBodyBodyGyroJerkMag_std=mean(fBodyBodyGyroJerkMag_std))
write.table(tidy_dataset_1, "UCI HAR Dataset/tidy_dataset.txt", row.names=F, quote=F, sep="\t")

