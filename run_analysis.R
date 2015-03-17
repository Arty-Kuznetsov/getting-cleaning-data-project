
run_analysis <- function() {
  data.base.dir <- "./data"
  data.dir <- paste(data.base.dir, "UCI HAR Dataset", sep="")
  get.dataset(data.base.dir)
  
  
}

# Retrieve and extract raw Samsung Smartphone data for cleansing
get.dataset <- function(base.dir) {
  zipfile <- paste(base.dir, "/dataset.zip", sep="")
  # Create data directory for downloaded files
  if (!file.exists(base.dir)) {
    dir.create(base.dir)
  }
  
  if (!file.exists(zipfile)) {
    download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', 
                  method='curl', destfile = zipfile)
  }
  
  unzip(zipfile, overwrite = TRUE, exdir = base.dir)
}