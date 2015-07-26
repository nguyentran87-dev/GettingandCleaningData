Peer Assessments /Getting and Cleaning Data Course Project
========================================

# Download data for the Project

The code to download the dataset for the project is inside the `download_data.R`. To dowload run

```
source('download_corpus.R')
```
This will download and unzip data.

# run_analysis.R

R script called `run_analysis.R` does the following. 

1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
3.  Uses descriptive activity names to name the activities in the data set
4.  Appropriately labels the data set with descriptive variable names. 
5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# how to run the analysis

```
source('run_analysis.R')
```

# Result 

The end result will be a file called `result_tidy_dataset.txt`.