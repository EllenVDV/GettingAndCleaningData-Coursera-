##Collect URL for all the datasources we need to get training and test results

setwd("C:\\DATA\\BI Project\\ADAPT program\\Module 3 (Coursera)\\Week 4\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")
urlSubjectTrain <- paste(getwd(),"/train/subject_train.txt",sep="")
urlSubjectTest <- paste(getwd(),"/test/subject_test.txt",sep="")
urlActivityTrain <- paste(getwd(),"/train/Y_train.txt",sep="")
urlActivityTest <- paste(getwd(),"/test/Y_test.txt",sep="")
urlTrain <- paste(getwd(),"/train/X_train.txt",sep="")
urlTest <- paste(getwd(),"/test/X_test.txt",sep="")

##Read the training and test results into data frames

SubjectTrain <- read.table(urlSubjectTrain)
SubjectTest <- read.table(urlSubjectTest)
ActivityTrain <- read.table(urlActivityTrain)
ActivityTest <- read.table(urlActivityTest)

dfTrain <- read.table(urlTrain)
dtTrain <- data.table(dfTrain)
dfTest <- read.table(urlTest)
dtTest <- data.table(dfTest)

subject <- rbind(SubjectTrain,SubjectTest)
subject <- setNames(subject,"subjectID")
activity <- setNames(activity,"ActivityID")

dt <- rbind(dtTrain, dtTest)
dt1 <- cbind(subject,activity)
ddt <- cbind(dt1, dt)
dt <- ddt

##Collect the features data

urlFeatures <- paste(getwd(),"/features.txt",sep="")
features <- read.table(urlFeatures)
features <- setNames(features,c("FeatureID","Feature"))

##Only extract the feature information about mean and standard deviations

f2 <- features[grep("mean\\(\\)|std\\(\\)", features$Feature),c("FeatureID","Feature")]
f3 <- cbind(f2,FeatureNr=paste("V",f2$FeatureID,sep=""))
f <- f3$FeatureNr
f <- as.character(f)
dt3 <- select(dt, c("subjectID","ActivityID",f))


##Collect the activity information

urlActivityLabels <- paste(getwd(),"/activity_labels.txt",sep="")
activitylabels <- read.table(urlActivityLabels)
activitylabels <- setnames(activitylabels,c("ActivityID", "ActivityLabel"))
dt4 <- merge(dt3, activitylabels, by="ActivityID", all.x=TRUE)
dt4 <- as.data.table(dt4)

setkey(dt4, subjectID, ActivityID, ActivityLabel)

##All the feature columns will now be transformed into rows, the column FeatureNr represents the feature
dt5 <- data.table(melt(dt4, key(dt4), variable.name="FeatureNr"))
dt5$activity <- factor(dt5$ActivityLabel)

##Add feature description to the data table
dt6 <- select(dt5,1:6)
dt6 <- merge(dt6,f3,by="FeatureNr")


#Add new columns based on the data in column Feature

dt6$Domain <- ifelse(grepl("^t", dt6$Feature, ignore.case = T), "Time", 
                  ifelse(grepl("^f", dt6$Feature, ignore.case = T), "Freq", ""))

dt6$Acceleration <- ifelse(grepl("BodyAcc", dt6$Feature, ignore.case = T), "Body", 
                     ifelse(grepl("GravityAcc", dt6$Feature, ignore.case = T), "Gravity", ""))

dt6$Instrument <- ifelse(grepl("Acc", dt6$Feature, ignore.case = T), "Accelerometer", 
                           ifelse(grepl("Gyro", dt6$Feature, ignore.case = T), "Gyroscope", ""))

dt6$Jerk <- ifelse(grepl("Jerk", dt6$Feature, ignore.case = T), "Jerk", "")

dt6$Magnitude <- ifelse(grepl("Mag", dt6$Feature, ignore.case = T), "Magnitude", "")

dt6$Variabele <- ifelse(grepl("-mean", dt6$Feature, ignore.case = T), "Mean", 
                         ifelse(grepl("-std", dt6$Feature, ignore.case = T), "Std", ""))

dt6$Axis <- ifelse(grepl("X$", dt6$Feature, ignore.case = T), "X", 
                        ifelse(grepl("Y$", dt6$Feature, ignore.case = T), "Y", 
                        ifelse(grepl("Z$", dt6$Feature, ignore.case = T), "Z", "")))

##Remove columns who are no longer needed

dt6$FeatureNr <- NULL
dt6$ActivityID <- NULL
dt6$FeatureID <- NULL
dt6$Feature <- NULL
dt6$ActivityLabel <- NULL

##Create two columns (count and average)
##Those columns contain a calculation based on the key of the data table

setkey(dt6, subjectID, activity,Domain,Acceleration,Instrument,Jerk,Magnitude,Variabele,Axis)
dtResult <- dt6[, list(count = .N, average = mean(value)), by=key(dt6)]
dtResult <- setnames(dtResult,"subjectID","subject")