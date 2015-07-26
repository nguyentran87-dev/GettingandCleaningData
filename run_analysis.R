#	1.  Merges the training and the test sets to create one data set.
get_merged_X_dataset <- function() {
	X_from_test <-read.table("./UCI HAR Dataset/test/X_test.txt")
	X_from_train <-read.table("./UCI HAR Dataset/train/X_train.txt")
	X_merged <- rbind(X_from_train, X_from_test)
}

get_merged_y_dataset <- function() {
	y_from_test <-read.table("./UCI HAR Dataset/test/y_test.txt")
	y_from_train <-read.table("./UCI HAR Dataset/train/y_train.txt")
	y_merged <- rbind(y_from_train, y_from_test)
}

get_subject_dataset <- function() {
	subject_from_test <-read.table("./UCI HAR Dataset/test/subject_test.txt")
	subject_from_train <-read.table("./UCI HAR Dataset/train/subject_train.txt")
	subject_merged <- rbind(subject_from_train, subject_from_test)
	names(subject_merged) <- "subject"
	return(subject_merged)
}

subject_dataset <- get_subject_dataset()
X_dataset <- get_merged_X_dataset()
y_dataset <- get_merged_y_dataset()

#	2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
get_selected_measurements <- function() {
	features <- read.table('./UCI HAR Dataset/features.txt', col.names=c('id', 'name'))
	features_selected_column <- grep('mean\\(\\)|std\\(\\)', features$name)	
	filtered_dataset <- X_dataset[, features_selected_column]
	names(filtered_dataset) <- features[features$id %in% feature_selected_columns, 2]
	return(filtered_dataset)
}

X_filtered_dataset <- get_selected_measurements()

#	3.  Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', col.names=c('id', 'name'))

#	4.  Appropriately labels the data set with descriptive variable names. 
y_dataset[, 1] = activity_labels[y_dataset[, 1], 2]
names(y_dataset) <- "activity"
combined_dataset <-cbind(subject_dataset, y_dataset, X_filtered_dataset)
write.csv(combined_dataset, "result_data.csv")
#	5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
measurements <- combined_dataset[, 3:length(combined_dataset)]
tidy_dataset <- aggregate(measurements, list(combined_dataset$subject, combined_dataset$activity), mean)
names(tidy_dataset)[1:2] <- c('subject', 'activity')
write.table(tidy_dataset, "result_tidy_dataset.txt", row.name=FALSE)


