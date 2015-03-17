library(dplyr)
library(tidyr)

# This function assumes UCI HAR Dataset sub-directory containing all Samsung instrumentation data
# exists within the same directory as this source file.
# Download from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
run_analysis <- function() {
  data.dir <- "./UCI HAR Dataset/"
  
  test.data <- read.datasets(data.dir, "test")
  train.data <- read.datasets(data.dir, "train")
}

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