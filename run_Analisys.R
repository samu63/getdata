# run_analysis.R
#
# Should be run in the same directory with "UCI HAR Dataset" directory
#
# The program doesoes the following.
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation
# for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.
#
#
#

library(data.table)

#set workdir:
#put the local path
#setwd("")
#
# START Merge the training and the test sets and create one data set
if (!file.exists("UCI HAR Dataset/test/X_test.txt")) stop ("test/X_test.txt not found.")
if (!file.exists("UCI HAR Dataset/train/X_train.txt")) stop ("train/X_train.txt not found.")

if (!file.exists("TData")) {
  dir.create("TData")
}

test_5rows <- read.table("UCI HAR Dataset/test/X_test.txt", nrows=5)
classes <- sapply(test_5rows, class)

test <- read.table("UCI HAR Dataset/test/X_test.txt", colClasses=classes)
train <- read.table("UCI HAR Dataset/train/X_train.txt", colClasses=classes)

tidy <- rbind(train, test)

test <- NULL
train <- NULL

# End point 1
############################################################################


# START Extracts only the measurements on the mean and standard deviation
# for each measurement.
features_names <- read.table("UCI HAR Dataset/features.txt")

colnames(tidy) <- features_names$V2

g <- grepl( "mean\\(\\)|std\\(\\)", features_names$V2, ignore.case=T)

#tidy1 <- subset(tidy, select=features_names[g,][,1])
tidy <- subset(tidy, select=features_names[g,][,1])

# END point 2
############################################################################

# START Uses descriptive activity names to name the activities in the data set
if (!file.exists("UCI HAR Dataset/test/y_test.txt")) stop ("test/y_test.txt not found.")
if (!file.exists("UCI HAR Dataset/train/y_train.txt")) stop ("train/y_train.txt not found.")

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

y_tidy <- rbind(y_train, y_test)

activity_names <- read.table("UCI HAR Dataset/activity_labels.txt")

for (num in activity_names$V1) {
  y_tidy[y_tidy == num] <- as.character(activity_names$V2[num])
}

tidy <- cbind(y_tidy, tidy)
#END point 3
############################################################################


# START Appropriately labels the data set with descriptive activity names.
if (!file.exists("UCI HAR Dataset/test/subject_test.txt")) stop ("test/subject_test.txt not found.")
if (!file.exists("UCI HAR Dataset/train/subject_train.txt")) stop ("train/subject_train.txt not found.")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

subject_tidy <- rbind(subject_train, subject_test)

tidy <- cbind(subject_tidy, tidy)

colnames(tidy)[1] <- "subjectID"
colnames(tidy)[2] <- "activity_name"
#END point 4
############################################################################


# START Creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.


tidy <- data.table(tidy)
setkey(tidy, subjectID, activity_name)

tidy.stats <- tidy[, lapply(.SD, mean), by=list(subjectID, activity_name) ]
write.csv(data.frame(tidy.stats),file="TData/tidydatafin.csv")

#END point 5
############################################################################
