#Set working directory
	setwd("C:/Users/sarahruthlang/Documents/R")


#4.Appropriately labels the data set with descriptive variable names. 
	#read in training and test sets with column names from features

	features <- read.table("features.txt")
	feat_trans <- t(as.vector(features["V2"]))

	Sub_Test <- read.table("subject_test.txt", col.names = "Subject")
	Sub_Train <- read.table("subject_train.txt", col.names = "Subject")	
	y_train <- read.table("y_train.txt", col.names = ("activ_label"))
	x_train <- read.table("x_train.txt", col.names = (feat_trans))
	x_test <- read.table("x_test.txt", col.names = (feat_trans))
	y_test <- read.table("y_test.txt", col.names = ("activ_label"))

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 

	x_train_mean <- x_train[,grep("mean", colnames(x_train))]
		#remove meanFreq variables
	x_train_mean2 <- x_train_mean[,grep("meanF", colnames(x_train_mean), invert = TRUE)]
	x_train_std <- x_train[,grep("std", colnames(x_train))]
	x_test_mean <- x_test[,grep("mean", colnames(x_test))]
		#remove meanFreq variables
	x_test_mean2 <- x_test_mean[,grep("meanF", colnames(x_test_mean), invert = TRUE)]
	x_test_std <- x_test[,grep("std", colnames(x_test))]

 
#1.Merges the training and the test sets to create one data set.

	#cbind training sets together
	train_data <- cbind(Sub_Train, y_train, x_train_mean2, x_train_std)

	#cbind test sets together
	test_data <- cbind(Sub_Test, y_test, x_test_mean2, x_test_std)

	#rbind test and training sets together
	feature_data <- rbind(train_data, test_data)


#3.Uses descriptive activity names to name the activities in the data set

	#import activity labels
	activity_labels <- read.table("activity_labels.txt", col.names = c("activ_label","activity_desc"))

	#merge activity labels 
	mergedData <- merge(feature_data, activity_labels, by.x="activ_label", by.y="activ_label", all=TRUE)


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

	#aggregate by subject and activity
	agg_set <- aggregate(mergedData[,3:68], by =list(mergedData$activity_desc, mergedData$Subject), mean)

	#rename grouping variables
	names(agg_set)[names(agg_set)=="Group.1"] <- "Activity_Desc"
	names(agg_set)[names(agg_set)=="Group.2"] <- "Subject"



#Extracts dataset for submission
	write.table(agg_set, "C:/Users/sarahruthlang/Documents/R/GettingAndCleaningData.txt", quote = FALSE, sep = "\t", row.names = FALSE)