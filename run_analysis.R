run_analysis <- function()
{
  
  
  #STEP 1 : Merges the training and the test sets to create one data set.
  #STEP 2 : Extracts only the measurements on the mean and standard deviation for each measurement.
  #STEP 3 : Uses descriptive activity names to name the activities in the data set
  #STEP 4 : Appropriately labels the data set with descriptive variable names.
  #STEP 5 : From the data set in step 4, creates a second, independent tidy data set with 
  #the average of each variable for each activity and each subject.
  
  setwd("C:\\GitHubRepo\\Getting_Cleaning_Data_Project")
  #*******************************************************
  #STEP 1 : Read the training and test files and merge them
  #*******************************************************  
  traindata = read.csv("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  traindata[,562] = read.csv("./UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
  traindata[,563] = read.csv("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  
  testdata = read.csv("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  testdata[,562] = read.csv("./UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
  testdata[,563] = read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  
  activityLabels = read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  featuresdata = read.csv("./UCI HAR Dataset/features.txt", sep="", header=FALSE)
  
  #Extracts only the measurements on the mean and standard deviation for each measurement.
  #Uses descriptive activity names to name the activities in the data set
  featuresdata[,2] = gsub('-std', 'Std', featuresdata[,2])
  featuresdata[,2] = gsub('-mean', 'Mean', featuresdata[,2])
  featuresdata[,2] = gsub('[-()]', '', featuresdata[,2])
  
  
  # Merge data sets here
  MergedData = rbind(traindata, testdata)
  
  
  #Extracts only the measurements on the mean and standard deviation for each measurement.
  measuresdata <- grep(".*Mean.*|.*Std.*", featuresdata[,2])
  
  #get the required row/columns
  featuresdata <- featuresdata[measuresdata,]
  measuresdata <- c(measuresdata, 562, 563)
  
  
  MergedData <- MergedData[,measuresdata]
  
  
  # read the features data
  colnames(MergedData) <- c(featuresdata$V2, "Activity", "Subject")
  colnames(MergedData) <- tolower(colnames(MergedData))
  currentActivity = 1
  for (currentActivityLabel in activityLabels$V2) {
    MergedData$activity <- gsub(currentActivity, currentActivityLabel, MergedData$activity)
    currentActivity <- currentActivity + 1
  }
  MergedData$activity <- as.factor(MergedData$activity)
  MergedData$subject <- as.factor(MergedData$subject)
  
  #Calculate the average of each variable for each activity and each subject.
  tidydata = aggregate(MergedData, by=list(activity = MergedData$activity, subject=MergedData$subject), mean, na.action = na.omit )
  
  
  write.table(tidydata, "tidydata.txt", row.name = FALSE)
}