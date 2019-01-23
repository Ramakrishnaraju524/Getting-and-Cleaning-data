##Load libraries
library(data.table)
library(dplyr)


##Downloads file, unzips and creates directories and datasets to work with. 
if(!file.exists("./data")){dir.create("./data")}
testzip <- " https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(testzip,destfile="./data/UCIHAR.zip")
unzip(zipfile="./data/Dataset.zip",exdir="./data")
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","activitydata"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "ppldata")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$activitydata)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "actcode")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "ppldata")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$activitydata)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "actcode")
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("actcode", "activity"))

## Merges the training and the test sets to create one data set.
xdata <- rbind(x_test, x_train)
ydata <- rbind(y_test, y_train)
ppldata <- rbind(subject_test, subject_train)
datamerge <- cbind(ppldata, ydata, xdata)

## Extracts only the measurements on the mean and standard deviation for each measurement.
Tidydata <- datamerge %>% select(ppldata, actcode, contains("mean"), contains("std"))

## Uses descriptive activity names to name the activities in the data set.
labels <- as.character(labels[,2])
Tidydata$activity <- labels[Tidydata$activity]

##Appropriately labels the data set with descriptive variable names.
names(Tidydata)[2] = "activity"
names(Tidydata)<-gsub("Acc", "Accelerometer", names(Tidydata))
names(Tidydata)<-gsub("angle", "Angle", names(Tidydata))
names(Tidydata)<-gsub("BodyBody", "Body", names(Tidydata))
names(Tidydata)<-gsub("^f", "Frequency", names(Tidydata))
names(Tidydata)<-gsub("-freq()", "Frequency", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("gravity", "Gravity", names(Tidydata))
names(Tidydata)<-gsub("Gyro", "Gyroscope", names(Tidydata))
names(Tidydata)<-gsub("Mag", "Magnitude", names(Tidydata))
names(Tidydata)<-gsub("-mean()", "Mean", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-std()", "STD", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("^t", "Time", names(Tidydata))
names(Tidydata)<-gsub("tBody", "TimeBody", names(Tidydata))

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata_2<- aggregate(. ~ppldata + activity, Tidydata, mean)
tidydata_2<- tidydata_2[order(tidydata_2$ppldata, tidydata_2$activity),]

write.csv(tidydata_2, file = "tidydata.csv")

