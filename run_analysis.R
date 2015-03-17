library(dplyr)
library(tidyr)

# This function assumes UCI HAR Dataset sub-directory containing all Samsung instrumentation data
# exists within the same directory as this source file.
# Download from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
run_analysis <- function() {
  data.dir <- "./UCI HAR Dataset/"
  features.file <- paste(data.dir, "features.txt", sep="")
  activity.file <- paste(data.dir, "activity_labels.txt", sep="")
  
  # Load raw data sets for training and test
  test.data <- read.datasets(data.dir, "test")
  train.data <- read.datasets(data.dir, "train")
  # Load sub-set of desired features (std|mean) and all activity labels
  features <- get.features(features.file)
  activity.labels <- get.activities(activity.file)
  
  # Combine rows for test and training X, Y and Subject results
  x.data <- bind_rows(test.data$X, train.data$X)
  y.data <- bind_rows(test.data$Y, train.data$Y)
  subject.data <- bind_rows(test.data$Subject, train.data$Subject)
  
  # Select only desired feature columns from x.data
  x.data <- x.data %>% select(num_range("V", features$FeatureLabel))
  names(x.data) <- features$FeatureName
  x.data <- clean.featureNames(x.data)
  
  # Update the y.data table to use correct activity names
  names(y.data) <- c("ActivityId")
  y.data <- inner_join(y.data, activity.labels, by="ActivityId")
  
  # Update the subject.data
  names(subject.data) <- c("Subject")
  
  # Combine final data set and return summary of mean values by activity and subject
  combined.data <- bind_cols(x.data, y.data, subject.data)
  tidy.data <- summarise_each(group_by(combined.data, ActivityName, Subject), funs(mean))
  
  # Trim off the final activity id column
  return(tidy.data[,1:81])
}

# Retrieve only the desired feature values (containing mean or std)
get.features <- function(features.file) {
  features <- tbl_df(read.table(features.file, 
                                header = FALSE, 
                                col.names = c("FeatureLabel", "FeatureName")))
  result <- features %>% filter(grepl("(std|mean)+", FeatureName))
  return (result)
}

get.activities <- function(activity.file) {
  activity.labels <- tbl_df(read.table(activity.file, 
                                       header = FALSE, 
                                       col.names = c("ActivityId", "ActivityName")))
  return(activity.labels)
}

# Put feature labels into a human readable format
clean.featureNames <- function(data) {
  names(data) <- gsub('^t', 'Time', names(data))
  names(data) <- gsub('^f', 'Frequency', names(data))
  names(data) <- gsub('\\(', '', names(data))
  names(data) <- gsub('\\)', '', names(data))
  names(data) <- gsub('-', '', names(data))
  names(data) <- gsub('mean', 'Mean', names(data))
  names(data) <- gsub('std', 'Std', names(data))
  return(data)
}

# Function to read in each raw dataset (either train or test)
# Returns a list labeled X, Y, Subject for each  corresponding file 
# within the specified sub-directory.
read.datasets <- function(data.dir, type) {
  if (type != "test" && type != "train") {
    stop("Invalid dataset. Must be 'training' or 'test'.")
  }
  
  x.filename <- paste(data.dir, type, "/X_", type, ".txt", sep="")
  y.filename <- paste(data.dir, type, "/Y_", type, ".txt", sep="")
  subject.filename <- paste(data.dir, type, "/subject_", type, ".txt", sep="")
  
  x.data <- tbl_df(read.table(x.filename))
  y.data <- tbl_df(read.table(y.filename))
  subject.data <- tbl_df(read.table(subject.filename))
  return (list("X" = x.data, "Y" = y.data, "Subject" = subject.data))
}