# Coursera project: Getting and cleaning data

setwd("R_projects/R_programming_assignment/course3_project")

## 1. Download and unzip the file

if(!file.exists("DATA"))
{
  dir.create("DATA")
}

if(!file.exists("DATA/UCI HAR Dataset"))
{
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                destfile = "DATA/UCI_HAAR_DATA.zip", method="curl")
  unzip("DATA/UCI_HAAR_DATA.zip", exdir = "DATA")
}

# load libraries
library(data.table)
library(dplyr)

# read the files
# loading feature file
features <- read.table("DATA/UCI HAR Dataset/features.txt")
colnames(features) <- c("Id", "feature")
df_features <- tbl_df(features)

## identifying mean and std in features
## using descriptive names for the activities

df_features <- mutate(df_features, Variables = make.names(df_features$feature, unique = TRUE))
df_features[, "Variables"] <- gsub("^t", "Time.", df_features$Variables)
df_features[, "Variables"] <- gsub("^f", "Frequency.", df_features$Variables)
df_features[, "Variables"] <- gsub("^angle", "Angle.", df_features$Variables)
df_features[, "Variables"] <- gsub("Acc", ".Acceleration", df_features$Variables)
df_features[, "Variables"] <- gsub("Jerk", ".Jerk", df_features$Variables)
df_features[, "Variables"] <- gsub("Gravity", ".Gravity", df_features$Variables)
df_features[, "Variables"] <- gsub("Mag", ".Magnitude", df_features$Variables)
df_features[, "Variables"] <- gsub("BodyBody", "Body",df_features$Variables)
df_features[, "Variables"] <- gsub("Gyro", ".Gyro",df_features$Variables)
df_features[, "Variables"] <- gsub("\\.\\.", ".",df_features$Variables)
df_features[, "Variables"] <- gsub("\\.\\.", ".",df_features$Variables)

# load activity labels file 
df_activities <- read.table("DATA/UCI HAR Dataset/activity_labels.txt", 
                            col.names = c("Id","Activity"))
df_activities <- tbl_df(df_activities)

## loading train files
train_X <- tbl_df(read.table("DATA/UCI HAR Dataset/train/X_train.txt"))
colnames(train_X) <- df_features$Variables

train_sub <- tbl_df(read.table("DATA/UCI HAR Dataset/train/subject_train.txt"))
train_sub <- rename(train_sub, Sub.Id = V1)

train_Y  <- tbl_df(read.table("DATA/UCI HAR Dataset/train/y_train.txt"))
train_Y <- rename(train_Y, Activity.Id = V1)

## combining train files 
train <- cbind(train_sub, train_Y, train_X)

## Loading Test files 

test_X <- tbl_df(read.table("DATA/UCI HAR Dataset/test/X_test.txt"))
colnames(test_X) <- df_features$Variables

test_sub <- tbl_df(read.table("DATA/UCI HAR Dataset/test/subject_test.txt"))
test_sub <- rename(test_sub, Sub.Id = V1)

test_Y  <- tbl_df(read.table("DATA/UCI HAR Dataset/test/y_test.txt"))
test_Y <- rename(test_Y, Activity.Id = V1)

## combine test files 
test <- cbind(test_sub, test_Y, test_X)


# merging test and train files 
merged <- rbind(train, test)

cat("Test file size: ",dim(test), "\nTrain file size: ", dim(train),
    "\n Merged file dimension:", dim(merged))

merged <- left_join(merged, df_activities, by=c("Activity.Id"="Id"))

# select the columns with mean() or std()
col1 <- c(grep("std()", colnames(merged)))
col2 <- c(grep("mean()", colnames(merged)))
selected_cols <- c(col1, col2)

merged_selected <- select(merged, Sub.Id, Activity_Id,Activity, selected_cols)
merged_selected <- group_by(merged_selected, Sub.Id, Activity)
## save tidy data
tidy_summary <- summarise_each(merged_selected, funs(mean))
write.table(tidy_summary, "tidy_summary.txt", row.names = FALSE)
