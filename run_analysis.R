##Load dplyr and data.table packages, if not installed run install.packages("dplyr")
##and install.packages("data.table") and run script again
library("dplyr")
library("data.table")

##Download Data & Unzip
if(!file.exists("Course4Data")){dir.create("Course4Data")}

DataURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(DataURL, destfile = "Course4Data.zip")

unzip(zipfile = "Course4Data.zip", exdir = "Course4Data")

##Read Files
features<-read.table("Course4data/UCI HAR Dataset/features.txt")
activities<-read.table("Course4data/UCI HAR Dataset/activity_labels.txt")
subject_test<-read.table("Course4Data/UCI HAR Dataset/test/subject_test.txt")
X_test<-read.table("Course4Data/UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("Course4Data/UCI HAR Dataset/test/y_test.txt")
subject_train<-read.table("Course4Data/UCI HAR Dataset/train/subject_train.txt")
X_train<-read.table("Course4Data/UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("Course4Data/UCI HAR Dataset/train/y_train.txt")


##Merges the training and the test sets to create one data set.
train<-cbind(subject_train, y_train,X_train)
test<-cbind(subject_test, y_test,X_test)
total<-rbind(subject, test, train)
names(total)<-c("subject", "event", as.character(features[,2]))

## remove duplicate columns.
total_unique<-total[,!duplicated(colnames(total))]
                
##Extracts only the measurements object the mean and standard deviation for each measurement.
mean_std<-total_unique%>%select(subject, event, contains("mean"), contains("std"))

##Uses descriptive activity names to name the activities in the data set
mean_std$event<-activities[mean_std$event, 2]

##Appropriately labels the data set with descriptive variable names.
names(mean_std)<-gsub("^t", "Time ", names(mean_std))
names(mean_std)<-gsub("^f", "Frequency ", names(mean_std))
names(mean_std)<-gsub("Acc", " Accelerometer ", names(mean_std))
names(mean_std)<-gsub("Gyro", " Gyroscope ", names(mean_std))
names(mean_std)<-gsub("Mag", " Magnitude ", names(mean_std))
names(mean_std)<-gsub("mean", "Mean", names(mean_std))
names(mean_std)<-gsub("std", "Std", names(mean_std))
##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

DataSet2<-mean_std%>%group_by(subject, event)%>%summarize_all(funs(mean))
View(DataSet2)
