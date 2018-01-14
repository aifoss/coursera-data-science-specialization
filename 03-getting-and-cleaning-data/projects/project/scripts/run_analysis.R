################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 3 Getting and Cleaning Data
## Week: Week 4
## Assignment: Getting and Cleaning Data Course Project
## File: run_analysis.R
## Date: 2016-01-22
################################################################################


################################################################################
# The purpose of this project is to demonstrate your ability to collect, 
# work with, and clean a data set. The goal is to prepare tidy data that can be 
# used for later analysis. You will be graded by your peers on a series of 
# yes/no questions related to the project. You will be required to submit: 
# 1) a tidy data set as described below, 2) a link to a Github repository 
# with your script for performing the analysis, and 3) a code book that describes 
# the variables, the data, and any transformations or work that you performed 
# to clean up the data called CodeBook.md. You should also include a README.md 
# in the repo with your scripts. This repo explains how all of the scripts work 
# and how they are connected.
#
# One of the most exciting areas in all of data science right now is 
# wearable computing - see for example this article . Companies like Fitbit, Nike, 
# and Jawbone Up are racing to develop the most advanced algorithms to attract 
# new users. The data linked to from the course website represent data collected 
# from the accelerometers from the Samsung Galaxy S smartphone. A full description 
# is available at the site where the data was obtained:
#    
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# Here are the data for the project:
#    
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
################################################################################


################################################################################
# Step 0: Download dataset and load it into R
################################################################################

# Step 0.1: Download and extract dataset zip file

print("Step 0.1: Downloading dataset...")
cat("\n")

# Step 0.1.1: Clear existing R objects from the workspace
rm(list=ls())

# Step 0.1.2: Create a directory to place the dataset
data_dir <- "data"
if (!file.exists(data_dir)) {
    dir.create(data_dir)
}

# Step 0.1.2: Download the dataset zip file and extract the contents
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "project_data.zip"
file_sep <- "/"
zipfile <- paste(data_dir, filename, sep=file_sep)
method <- "curl"

download.file(url, zipfile, method)
unzip(zipfile, exdir=data_dir)
file.remove(zipfile)

# Step 0.2: Load data into R objects

print("Step 0.2: Loading data into R...")
cat("\n")

# Step 0.2.1: Load data into the workspace
filelist <- list.files(path=data_dir)
dataset_name <- filelist[1]
dataset_dir <- paste(data_dir, dataset_name, sep=file_sep)
train_data_dir <- paste(dataset_dir, "train", sep=file_sep)
test_data_dir <- paste(dataset_dir, "test", sep=file_sep)

activity_label_file <- paste(dataset_dir, "activity_labels.txt", sep=file_sep)
feature_label_file <- paste(dataset_dir, "features.txt", sep=file_sep)
train_activity_file <- paste(train_data_dir, "y_train.txt", sep=file_sep)
train_subject_file <- paste(train_data_dir, "subject_train.txt", sep=file_sep)
train_data_file <- paste(train_data_dir, "X_train.txt", sep=file_sep)
test_activity_file <- paste(test_data_dir, "y_test.txt", sep=file_sep)
test_subject_file <- paste(test_data_dir, "subject_test.txt", sep=file_sep)
test_data_file <- paste(test_data_dir, "X_test.txt", sep=file_sep)

activity_label_data <- read.table(activity_label_file)
feature_label_data <- read.table(feature_label_file)
train_activity_data <- read.table(train_activity_file)
train_subject_data <- read.table(train_subject_file)
train_data_original <- read.table(train_data_file)
test_activity_data <- read.table(test_activity_file)
test_subject_data <- read.table(test_subject_file)
test_data_original <- read.table(test_data_file)

################################################################################
# Step 1: Merge the training and the test sets to create one data set
################################################################################

print("Step 1: Merging the training and test datasets...")
cat("\n")

# Step 1.1: Label train/test data with feature names

feature_names <- as.matrix(feature_label_data[2])
colnames(train_data_original) <- feature_names[,1]
colnames(test_data_original) <- feature_names[,1]

# Step 1.2: Integrate subject and activity data into train/test data
# ----------------------------------------
# Subject | Activity | ... (Features) ...
# ----------------------------------------

train_data <- cbind(train_subject_data, train_activity_data, train_data_original)
test_data <- cbind(test_subject_data, test_activity_data, test_data_original)
subject_activity_headers <- c("subject", "activity")
colnames(train_data)[1:2] <- subject_activity_headers
colnames(test_data)[1:2] <- subject_activity_headers

# Step 1.3: Merge train and test data

merged_data <- rbind(train_data, test_data)

################################################################################
# Step 2: Extract only the measurements on the mean and standard deviation
################################################################################

print("Step 2: Extracting only the mean/std measurements...")
cat("\n")

# Step 2.1: Extract the names of columns to be included

colnames <- colnames(merged_data)
std_name_indices <- grep("-std()", colnames)
mean_name_indices <- grep("-mean()", colnames)
mean_freq_name_indices <- grep("-meanFreq()", colnames)
std_names <- colnames[std_name_indices]
mean_names <- colnames[mean_name_indices]
mean_freq_names <- colnames[mean_freq_name_indices]
selected_colnames <- colnames[
    (colnames %in% std_names | colnames %in% mean_names)
    &!colnames %in% mean_freq_names]
selected_colnames <- append(subject_activity_headers, selected_colnames, 2)

# Step 2.2: Extract a subset of data containing mean/std measurements

subset <- merged_data[selected_colnames]

################################################################################
# Step 3: Use descriptive activity names to name the activities in the dataset
################################################################################

print("Step 3: Replacing activity IDs with activity names...")
cat("\n")

# Step 3.1: Extract activity IDs and names

activity_ids <- as.vector(activity_label_data$V1) 
activity_names <- as.vector(activity_label_data$V2)

# Step 3.2: Replace activity ID value with activity name value

for (i in 1:length(activity_ids)) {
    subset$activity[subset$activity == i] <- activity_names[i]
}

################################################################################
# Step 4: Label the dataset with descriptive variable names
################################################################################

print("Step 4: Changing variable names to descriptive names...")
cat("\n")

# Step 4.1: Replace variable names with more descriptive ones

variable_names <- colnames(subset)
variable_names <- gsub("tBody", "timed_body", variable_names)
variable_names <- gsub("tGravity", "timed_gravity", variable_names)
variable_names <- gsub("fBodyBody", "fast_fourier_transform_body", variable_names)
variable_names <- gsub("fBody", "fast_fourier_transform_body", variable_names)
variable_names <- gsub("Acc", "_acceleration", variable_names)
variable_names <- gsub("Jerk", "_jerk_signal", variable_names)
variable_names <- gsub("Gyro", "_gyroscope", variable_names)
variable_names <- gsub("Mag", "_magnitude", variable_names)
variable_names <- gsub("-mean\\(\\)", "_mean", variable_names)
variable_names <- gsub("-std\\(\\)", "_standard_deviation", variable_names)
variable_names <- gsub("-X", "_angle_x", variable_names)
variable_names <- gsub("-Y", "_angle_y", variable_names)
variable_names <- gsub("-Z", "_angle_z", variable_names)
colnames(subset) <- variable_names

# Step 4.2: Save the tidy dataset

tidy_dataset_1 <- subset
output_dir <- "output"
if (!file.exists(output_dir)) {
    dir.create(output_dir)
}
filename <- "tidy_dataset_1.txt"
output_file <- paste(output_dir, filename, sep=file_sep)
write.table(tidy_dataset_1, file=output_file, row.names=FALSE)

# Step 4.3: Save the descriptive variable names for later use in the CodeBook

markdown_dir <- "markdown"
if (!file.exists(markdown_dir)) {
    dir.create(markdown_dir)
}
filename <- "variable_names.txt"
varname_file <- paste(output_dir, filename, sep=file_sep)
write.table(variable_names, file=varname_file, row.names=FALSE, col.names="variable_name")

print("=====> Tidy dataset 1 saved")
cat("\n")

################################################################################
# Step 5: Create a second tidy dataset with the average per activity/subject
################################################################################

print("Step 5: Creating a second tidy dataset grouped by subject and activity...")
cat("\n")

# Step 5.1: Create a second tidy datset

if (!"reshape2" %in% rownames(installed.packages())) {
    install.packages("reshape2")
}
library(reshape2)

melted_data <- melt(subset, id.vars=subject_activity_headers)
aggregate_data <- dcast(melted_data,
                        subject+activity ~ variable,
                        fun.aggregate=mean)

# Step 5.2: Save the second tidy dataset

tidy_dataset_2 <- aggregate_data
filename <- "tidy_dataset_2.txt"
output_file <- paste(output_dir, filename, sep=file_sep)
write.table(tidy_dataset_2, file=output_file, row.names=FALSE)

print("=====> Tidy dataset 2 saved")
cat("\n")

################################################################################