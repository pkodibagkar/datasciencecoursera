## Getting and Cleaning Data Course Project

Prasad Kodibagkar

### Description
Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 
Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been 
video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the 
training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap 
(128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Step 1. Initialize common variables
- Sets the directory where data files are located
- Loads the activty labels

### Step 2. Create a data frame after merging subject, activity and measurement data files
A common function getMeasurements was created to merge the subject, activity and measurement data
from different files. The function takes the source directory and type of data (XXXXX) - test or train
- Subject Data - subject_XXXXX.txt
- Activity Data - y_XXXXX.txt
- Meaurement Data - X_XXXXX.txt

The function returns a data frame with only the Std deviation and Mean measurements. The standardized columns names are assigned to the data frame


### Step 3. Create a merged data set
In this step, the training and test data frames are created by calling the function. 
Next, they merged together to create a single data frame

### Step 4. Create a Tidy data frame by summarizing the totalMeasurementDF table to include just the mean of each variable for each subject and activity
The merged data frame is aggregated to calculate the mean of each variable by subject and activity
The activity name is merged into the tidy data frame

### Step 5. Write the Tidy Data Frame to a file
The tidy data frame is written to a file using row.names set to TRUE to remove the row numbers.
This file is created as a text file using space as a separator.

The structure of the MeasurementData.txt file generated is as follows

SubjectId
ActivityId
ActivityName
TimeBodyAccMean-X
TimeBodyAccMean-Y
TimeBodyAccMean-Z
TimeBodyAccStdDev-X
TimeBodyAccStdDev-Y
TimeBodyAccStdDev-Z
TimeGravityAccMean-X
TimeGravityAccMean-Y
TimeGravityAccMean-Z
TimeGravityAccStdDev-X
TimeGravityAccStdDev-Y
TimeGravityAccStdDev-Z
TimeBodyAccJerkMean-X
TimeBodyAccJerkMean-Y
TimeBodyAccJerkMean-Z
TimeBodyAccJerkStdDev-X
TimeBodyAccJerkStdDev-Y
TimeBodyAccJerkStdDev-Z
TimeBodyGyroMean-X
TimeBodyGyroMean-Y
TimeBodyGyroMean-Z
TimeBodyGyroStdDev-X
TimeBodyGyroStdDev-Y
TimeBodyGyroStdDev-Z
TimeBodyGyroJerkMean-X
TimeBodyGyroJerkMean-Y
TimeBodyGyroJerkMean-Z
TimeBodyGyroJerkStdDev-X
TimeBodyGyroJerkStdDev-Y
TimeBodyGyroJerkStdDev-Z
TimeBodyAccMagMean
TimeBodyAccMagStdDev
TimeGravityAccMagMean
TimeGravityAccMagStdDev
TimeBodyAccJerkMagMean
TimeBodyAccJerkMagStdDev
TimeBodyGyroMagMean
TimeBodyGyroMagStdDev
TimeBodyGyroJerkMagMean
TimeBodyGyroJerkMagStdDev
FrequencyBodyAccMean-X
FrequencyBodyAccMean-Y
FrequencyBodyAccMean-Z
FrequencyBodyAccStdDev-X
FrequencyBodyAccStdDev-Y
FrequencyBodyAccStdDev-Z
FrequencyBodyAccMeanFreq-X
FrequencyBodyAccMeanFreq-Y
FrequencyBodyAccMeanFreq-Z
FrequencyBodyAccJerkMean-X
FrequencyBodyAccJerkMean-Y
FrequencyBodyAccJerkMean-Z
FrequencyBodyAccJerkStdDev-X
FrequencyBodyAccJerkStdDev-Y
FrequencyBodyAccJerkStdDev-Z
FrequencyBodyAccJerkMeanFreq-X
FrequencyBodyAccJerkMeanFreq-Y
FrequencyBodyAccJerkMeanFreq-Z
FrequencyBodyGyroMean-X
FrequencyBodyGyroMean-Y
FrequencyBodyGyroMean-Z
FrequencyBodyGyroStdDev-X
FrequencyBodyGyroStdDev-Y
FrequencyBodyGyroStdDev-Z
FrequencyBodyGyroMeanFreq-X
FrequencyBodyGyroMeanFreq-Y
FrequencyBodyGyroMeanFreq-Z
FrequencyBodyAccMagMean
FrequencyBodyAccMagStdDev
FrequencyBodyAccMagMeanFreq
FrequencyBodyBodyAccJerkMagMean
FrequencyBodyBodyAccJerkMagStdDev
FrequencyBodyBodyAccJerkMagMeanFreq
FrequencyBodyBodyGyroMagMean
FrequencyBodyBodyGyroMagStdDev
FrequencyBodyBodyGyroMagMeanFreq
FrequencyBodyBodyGyroJerkMagMean
FrequencyBodyBodyGyroJerkMagStdDev
FrequencyBodyBodyGyroJerkMagMeanFreq
angle(tBodyAccMean,gravity)
angle(tBodyAccJerkMean),gravityMean)
angle(tBodyGyroMean,gravityMean)
angle(tBodyGyroJerkMean,gravityMean)
angle(X,gravityMean)
angle(Y,gravityMean)
angle(Z,gravityMean)
