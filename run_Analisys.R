# run_analysis.R
#
# Should be run in the same directory with "test" and "train" directories
#
#
#
#

library(data.table)

#set workdir:
#local path
#setwd("")
#
# START Merge the training and the test sets and create one data set
if (!file.exists("test/X_test.txt")) stop ("test/X_test.txt not found.")
if (!file.exists("train/X_train.txt")) stop ("train/X_train.txt not found.")

if (!file.exists("../TData")) {
  dir.create("../TData")
}

T10rows <- read.table("test/X_test.txt", nrows=10)
classes <- sapply(T10rows, class)

test <- read.table("test/X_test.txt", colClasses=classes)
train <- read.table("train/X_train.txt", colClasses=classes)

features_names <- read.table("features.txt")

if (!file.exists("test/y_test.txt")) stop ("test/y_test.txt not found.")
if (!file.exists("train/y_train.txt")) stop ("train/y_train.txt not found.")

y_test <- read.table("test/y_test.txt")
y_train <- read.table("train/y_train.txt")

if (!file.exists("test/subject_test.txt")) stop ("test/subject_test.txt not found.")
if (!file.exists("train/subject_train.txt")) stop ("train/subject_train.txt not found.")

subject_test <- read.table("test/subject_test.txt")
subject_train <- read.table("train/subject_train.txt")


tidy <- rbind(train, test)

test <- NULL
train <- NULL




# End point 1
############################################################################


# START Extracts only the measurements on the mean and standard deviation
# for each measurement.


colnames(tidy) <- features_names$V2

g <- grepl( "mean\\(\\)|std\\(\\)", features_names$V2, ignore.case=T)

#tidy1 <- subset(tidy, select=features_names[g,][,1])
tidy <- subset(tidy, select=features_names[g,][,1])

# END point 2
############################################################################

# START Uses descriptive activity names to name the activities in the data set

y_tidy <- rbind(y_train, y_test)

activity_names <- read.table("activity_labels.txt")

for (num in activity_names$V1) {
  y_tidy[y_tidy == num] <- as.character(activity_names$V2[num])
}

tidy <- cbind(y_tidy, tidy)
#END point 3
############################################################################


# START Appropriately labels the data set with descriptive activity names.

subject_tidy <- rbind(subject_train, subject_test)

tidy <- cbind(subject_tidy, tidy)

colnames(tidy)[1] <- "subjectID"
colnames(tidy)[2] <- "activity_name"
write.table(data.frame(tidy),file="../TData/tidydatastep1.txt")
#END point 4
############################################################################


# START Creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.


tidy <- data.table(tidy)
setkey(tidy, subjectID, activity_name)

tidy.final <- tidy[, lapply(.SD, mean), by=list(subjectID, activity_name) ]
write.table(data.frame(tidy.final),file="../TData/tidydatasytep2.txt")

#END point 5
############################################################################
