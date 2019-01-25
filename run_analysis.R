# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
# The script should do the following:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#    for each activity and each subject.

library("dplyr")
library("data.table")

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
x_test <- read.table("UCI HAR Dataset/test/x_test.txt", header = FALSE)

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
x_train <- read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE)

features_names <- read.table("UCI HAR Dataset/features.txt", header = FALSE)
activities <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Merge the data sets

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(y_train, y_test)
features <- rbind(x_train, x_test)

colnames(features) <- t(features_names[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# Extract mean and standard deviation

mean_std <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

extractedColumns <- c(mean_std, 562, 563)
dim(completeData)

extractedData <- completeData[,extractedColumns]
dim(extractedData)

# Appropriate labels

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activities[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)


# Second independent tidy set
extractedData$Subject <- as.factor(extractedData$Subject)
tidy_data <- data.table(extractedData)


tidy_data <- aggregate(. ~Subject + Activity, extractedData, mean)
tidy_data <- tidyData[order(tidyData$Subject,tidyData$Activity),]

write.table(tidy_data, file = "Tidy.txt", row.names = FALSE)
