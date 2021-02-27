# GettingandCleaningDataCourseAssignment

This repo contains the relevant files for the Getting and Cleaning Data course project.

Study information: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Raw data 
 https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  

Files

- run_analysis.R - script performing the data transformation (per assignment prompt):"
    1) Merges the training and the test sets to create one data set.
    2) Extracts only the measurements on the mean and standard deviation for each measurement. 
    3) Uses descriptive activity names to name the activities in the data set
    4) Appropriately labels the data set with descriptive variable names. 
    5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."

- codebook.rmd
    Defines variables & describes data transformations
    
- tidy.text - output file created from run_analysis.R script
    Contains the summarized tidy dataset, with the mean of each feature variable for each activity and each subject.
