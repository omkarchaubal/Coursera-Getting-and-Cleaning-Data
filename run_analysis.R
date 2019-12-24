library(plyr)
library(dplyr)

#1. Merges the training and the test sets to create one data set
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists("./getdata_projectfiles_UCI HAR Dataset.zip")) {
  download.file(fileURL, destfile = "./getdata_projectfiles_UCI HAR Dataset.zip")
}

if (!file.exists("./getdata_projectfiles_UCI HAR Dataset")) {
  unzip(zipfile = "./getdata_projectfiles_UCI HAR Dataset.zip", exdir = "./getdata_projectfiles_UCI HAR Dataset")
}



setwd("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")
colnames(activity_labels) <- c('activitylabel', 'activity')

setwd("./test")
test_dataset <- read.table("X_test.txt")
test_labels <- read.table("y_test.txt")
colnames(test_labels) <- "activitylabel"
test_subject <- read.table("subject_test.txt")
colnames(test_subject) <- "subject"
test <- cbind(test_dataset, test_labels, test_subject)

setwd("../")
setwd("./train")
train_dataset <- read.table("X_train.txt")
train_labels <- read.table("y_train.txt")
colnames(train_labels) <- "activitylabel"
train_subject <- read.table("subject_train.txt")
colnames(train_subject) <- "subject"
train <- cbind(train_dataset, train_labels, train_subject)

DF <- rbind(test, train)

#2. Extracts only the measurements on the mean and standard deviation for each measurement
DF<- DF[,c(grep("mean\\(|std", features$V2), 562, 563)]

#3. Uses descriptive activity names to name the activities in the data set
DF <- join(DF, activity_labels, by = "activitylabel")
DF$activitylabel <- NULL

#4.Appropriately labels the data set with descriptive variable names
colnames(DF)[1: (ncol(DF)-2)]<-as.character(features[grep("mean\\(|std", features$V2), 2])
colnames(DF) <- gsub("\\(\\)", "", colnames(DF))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
summary <- DF %>%
          group_by(activity, subject)%>%
          summarise_all(funs(mean))

colnames(summary) <- gsub("^t", "mean-t", colnames(summary))
colnames(summary) <- gsub("^f", "mean-f", colnames(summary))

setwd("../")
setwd("../")
setwd("../")
write.csv(summary, "summary.csv", row.names = FALSE)