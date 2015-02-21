library(plyr)
library(RCurl)
URL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "./data.zip", method="curl")
unzip("./data.zip")
## Merge tables in test folder
sTest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
xTest<-read.table("./UCI HAR Dataset/test/X_test.txt")
yTest<-read.table("./UCI HAR Dataset/test/y_test.txt")

testDB<-cbind(sTest,yTest,xTest)
# Extracting variable names from features.txt
namesDB<-read.table("./UCI HAR Dataset/features.txt")
nameList<-as.vector(namesDB$V2)
names(testDB)<-c("id","activity", nameList)
# Merging tables in train folder
sTrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
xTrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
trainDB<-cbind(sTrain,yTrain,xTrain)
names(trainDB)<-c("id","activity", nameList)

# Making the final table for processing (item 1 of the requirements)

stepOneDB<-rbind(testDB,trainDB)


## By this point we have the entire data set and all variables have appropriate names
## Now we want to only extract the variables that are supposed to be means and standard deviation of measurements.

allMeans<-stepOneDB[,grep('mean',names(stepOneDB))]

allSds<-stepOneDB[,grep('std',names(stepOneDB))]

## Table with all variables that are supposed to be means and standard deviation of measurements.

allMeanAndSds<-cbind(stepOneDB[,1:2],allMeans,allSds)

## For the sake of the dataset, we also tidy the entire data set, but will proceed with a smaller number of variables.

entireTidyTable<-aggregate(stepOneDB,by=list(stepOneDB$id,stepOneDB$activity),FUN=mean,na.rm=TRUE)

entireTidyTable<-tidyTable[,c(-1,-4)]

names(entireTidyTable)<-c("Activity", "SubjectId", nameList)

entireTidyTable$Activity<-factor(entireTidyTable$Activity)

levels(entireTidyTable$Activity)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")


## We also doing the same for ALL VARIABLES, altho I don't think that was the intended purpose of the exercise.

subTidyTable<-aggregate(allMeanAndSds,by=list(allMeanAndSds$id,allMeanAndSds$activity),FUN=mean,na.rm=TRUE)

subTidyTable<-subTidyTable[,c(-1,-4)]

names(subTidyTable)<-names(allMeanAndSds)

subTidyTable<-rename(subTidyTable, c("id" = "Activity", "activity"="SubjectId"))

subTidyTable$Activity<-factor(subTidyTable$Activity)

levels(subTidyTable$Activity)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

## From the first table, we are filtering the data to get the Body mean and Body standard deviation.

tm<-stepOneDB[,setdiff(grep('tBodyAcc-mean',names(stepOneDB)),grep('Mag',names(stepOneDB)))]
tsd<-stepOneDB[,setdiff(grep('tBodyAcc-std',names(stepOneDB)),grep('Mag',names(stepOneDB)))]

## Joining the body mean and body standard deviation tables with the subject Id and activity table

fdf<-cbind(stepOneDB[,1:2],tm,tsd)




#Calculating the mean for every pair of subject/activity (item 5 of the requirement)

tidyTable<-aggregate(fdf,by=list(fdf$id,fdf$activity),FUN=mean,na.rm=TRUE)

##Removing the extra columns created

tidyTable<-tidyTable[,c(-1,-4)]

## Setting the names properly (item 3 of the requirement)

names(tidyTable)<-c("Activity", "SubjectId", "AccelerometerMeanX","AccelerometerMeanY","AccelerometerMeanZ","AccelerometerSDX","AccelerometerSDY","AccelerometerSDZ")

tidyTable$Activity<-factor(tidyTable$Activity)

levels(tidyTable$Activity)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")


write.table(tidyTable, file="tidyTable.txt", sep=",", row.names = FALSE)