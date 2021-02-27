library(data.table)
library(dplyr)

#set working directory

setwd("/Users/peteralexander/Desktop/GettingandCleaningDataCourseAssignment")

# download & unzip data files

description <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#get training data
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

#get test data
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)

#get feature names & activity labels
featurenames <- read.table("UCI HAR Dataset/features.txt")

activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
colnames(activitylabels) <- c("activityid", "activity")

#combine test & train data for each dataset
subject <- rbind(subjecttest, subjecttrain)
activity <- rbind(ytest, ytrain)
features <- rbind(xtest, xtrain)

#asign column names 
colnames(features) <- t(featurenames[2])
colnames(activity) <- "activity"
colnames(subject) <- "subject"

## part 1: merge into single dataset ##
combined <- cbind(subject, activity, features)

## part 2: extract relevant data ##
#extract mean & standard deviation into a reduced dataset
meansdcols <- grep("subject|activity|.*[Mm]ean.*|.*[Ss]td.*", names(combined))
datameansd <- combined[ ,meansdcols]

## part 3: assign activity names to activity id ##
datameansd$activity <- as.character(datameansd$activity)
for (i in 1:6){
          datameansd$activity[datameansd$activity == i] <- as.character(activitylabels[i,2])
}

## part 4: label with descriptive variable names ##

names(datameansd) <- gsub("Mean", "mean", names(datameansd))
names(datameansd) <- gsub("-std", "standarddeviation", names(datameansd))
names(datameansd) <- gsub("Gyro", "gyroscope", names(datameansd))
names(datameansd) <- gsub("Gravity", "gravity", names(datameansd))
names(datameansd) <- gsub("Mag", "magnitude", names(datameansd))
names(datameansd) <- gsub("Acc", "accelerometer", names(datameansd))
names(datameansd) <- gsub("BodyBody", "body", names(datameansd))
names(datameansd) <- gsub("Body", "body", names(datameansd))
names(datameansd) <- gsub("Jerk", "jerk", names(datameansd))
names(datameansd) <- gsub("^t", "time", names(datameansd))
names(datameansd) <- gsub("^f", "frequency", names(datameansd))
names(datameansd) <- gsub("Freq", "frequency", names(datameansd))
names(datameansd) <- gsub("[[:punct:]]", "", names(datameansd))

## Create a second, independent tidy data set with mean of each variable 
## for each subject & activity

tidydata <- datameansd %>% 
          group_by(subject, activity) %>% 
          summarise_all(funs(mean))

# write new summary file 'tidy.txt'
write.table(tidydata, file = "tidy.txt", row.names = FALSE, quote = FALSE)
