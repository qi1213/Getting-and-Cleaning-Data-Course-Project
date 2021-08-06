## cleaning whereable data for a future project 

#librarys
library(utils)
library(stringr)
library(dplyr)
library(reshape2)
library(plyr)

#Preprocessing/getting data 
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "./downloaded_data.zip")

unzip("./downloaded_data.zip")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")


#Merge all training and test sets together to create one data set with all our 
#information
full_X <- rbind(X_test, X_train)
full_y <- rbind(y_test, y_train)
full_subject <- rbind(subject_test, subject_train)

#apply column names to each column 
colnames(full_X) <- features[,2]
colnames(full_y) <- "activity_performed"
colnames(full_subject) <- "subject_id"
colnames(activity_labels) <- c("linking_number","activity_name")

#reduce full_X variables to only those with mean or standard deviation 
mean_std_data <- full_X[, grep(pattern = "mean|std", colnames(full_X))]

#column bind the 3 train/test sets together 
full_data_uncleaned <- cbind(full_subject, full_y, mean_std_data)

#merge in activity names to full dataset and remove V1 variable with activity
#code
full_y_named <- merge(full_data_uncleaned, activity_labels, by.x = "activity_performed", by.y = "linking_number")
full_y_named <- select(full_y_named, -activity_performed)

#reorder data so subject and activity are first in the dataset 
clean_data <- relocate(full_y_named, subject_id, activity_name)

#rename columns to be more tidy 
colnames(clean_data) <- tolower(colnames(clean_data))
colnames(clean_data) <- str_replace(colnames(clean_data), "^t", "time_")
colnames(clean_data) <- str_replace(colnames(clean_data), "^f", "freq_")
colnames(clean_data) <- gsub("-", "_", colnames(clean_data))
colnames(clean_data) <- gsub("\\(\\)", "", colnames(clean_data))

#calculate the average of each variable for each activity and each subject 
consolidated_mean_data <- clean_data %>% 
                            group_by(subject_id, activity_name) %>%
                            dplyr::summarize(across(everything(), list(mean)))

#rename columns to be more tidy 
colnames(consolidated_mean_data) <- str_replace(colnames(consolidated_mean_data), "1", "average")
