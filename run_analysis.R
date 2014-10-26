# 1. Merges the training and the test sets to create one data set. 
# read training data from files
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt",col.names=c("subject_id"))
subjecttrain$ID <- as.numeric(rownames(subjecttrain))                          
Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
Xtrain$ID <- as.numeric(rownames(Xtrain))
Ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names=c("activity_id"))
Ytrain$ID <- as.numeric(rownames(Ytrain))
# merge training data into one data frame
train <- merge(subjecttrain,Ytrain, all=TRUE)
train <- merge(train, Xtrain, all=TRUE)
# read testing data
subjecttest = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id"))
subjecttest$ID <- as.numeric(rownames(subjecttest))
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
Xtest$ID <- as.numeric(rownames(Xtest))
Ytest <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names=c("activity_id"))
Ytest$ID <- as.numeric(rownames(Ytest))
# merge testing data into one data frame
test <- merge(subjecttest,Ytest, all=TRUE)
test <- merge(test, Xtest, all=TRUE)
# merge training and testing data
data <- rbind(train,test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)
subfeatures <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
data <- data[, c(c(1, 2, 3), subfeatures$feature_id + 3)]

# 3. Uses descriptive activity names to name the activities in the data set
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),)
data <- merge(data, activitylabels)

# 4. Appropriately labels the data set with descriptive variable names. 
subfeatures$feature_label = gsub("\\(\\)", "", subfeatures$feature_label)
subfeatures$feature_label = gsub("-", ".", subfeatures$feature_label)
for (i in 1:length(subfeatures$feature_label)) {
  colnames(data)[i + 3] <- subfeatures$feature_label[i]
}
data2 = data

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
names <- c("ID","activity_label")
data3 <- data2[,!(names(data2) %in% names)]
finaldata <-aggregate(data3, by=list(subject = data3$subject_id, activity = data3$activity_id), FUN=mean, na.rm=TRUE)
names <- c("subject","activity")
finaldata <- finaldata[,!(names(finaldata) %in% names)]
finaldata <- merge(finaldata, activitylabels)
write.csv(file="submit.csv", x=finaldata)
write.table(x=finaldata, file="courseproject.txt", row.name=FALSE)
