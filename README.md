---
title: "ReadMe"
author: "Bruno Bandeira de Azevedo"
date: "February 17, 2015"
output:
html_document:
theme: cosmo
pandoc_args: [
"+RTS", "-K64m",
"-RTS"
]
---

We will attempt with this Course Project to output a Tidy Data set that can be useful to analyse different patterns of users movement. This data was collected using a personal device.

##Loading the data

**First step is to load the data files onto R to process. To do this you we used the following commands:**


        ```r
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

        ###Making the final table for processing (item 1 of the requirements)

        stepOneDB<-rbind(testDB,trainDB)
        

## Processing the data

**This will retrieve all files from the server, unzip them, assign them to the proper variables. After that, we extract the names from the features.txt file and name each column appropriately. We finally merge the data sets into a final data set for processing.**


**Since it was not provided a explanation of which measurements may be most valuable and for the sake of clarity in the tidy data set, I have decided the most useful measurements will be those that are from the body accelerometers for time and also for each axis. Considering some movements might be better evaluated by having the value of all axis I decided to leave all the three axis inside the data frame and use their means and standard deviations.**

**The process to extract that data from the previous data frame is below.**


        ```r
        #### By this point we have the entire data set and all variables have appropriate names.
        #### Now we want to only extract the variables that are supposed to be means and standard deviation of measurements.

        allMeans<-stepOneDB[,grep('mean',names(stepOneDB))]

        allSds<-stepOneDB[,grep('std',names(stepOneDB))]

        ### Table with all variables that are supposed to be means and standard deviation of measurements.

        allMeanAndSds<-cbind(stepOneDB[,1:2],allMeans,allSds)

        ### For the sake of the dataset, we also tidy the entire data set, but will proceed with a smaller number of variables.

        entireTidyTable<-aggregate(stepOneDB,by=list(stepOneDB$id,stepOneDB$activity),FUN=mean,na.rm=TRUE)

        entireTidyTable<-tidyTable[,c(-1,-4)]

        names(entireTidyTable)<-c("Activity", "SubjectId", nameList)

        entireTidyTable$Activity<-factor(entireTidyTable$Activity)

        levels(entireTidyTable$Activity)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")


        ### We also doing the same for ALL VARIABLES, altho I don't think that was the intended purpose of the exercise.

        subTidyTable<-aggregate(allMeanAndSds,by=list(allMeanAndSds$id,allMeanAndSds$activity),FUN=mean,na.rm=TRUE)

        subTidyTable<-subTidyTable[,c(-1,-4)]

        names(subTidyTable)<-names(allMeanAndSds)

        subTidyTable<-rename(subTidyTable, c("id" = "Activity", "activity"="SubjectId"))

        subTidyTable$Activity<-factor(subTidyTable$Activity)

        levels(subTidyTable$Activity)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

        ### From the first table, we are filtering the data to get the Body mean and Body standard deviation.

        tm<-stepOneDB[,setdiff(grep('tBodyAcc-mean',names(stepOneDB)),grep('Mag',names(stepOneDB)))]
        tsd<-stepOneDB[,setdiff(grep('tBodyAcc-std',names(stepOneDB)),grep('Mag',names(stepOneDB)))]

        ### Joining the body mean and body standard deviation tables with the subject Id and activity table

        fdf<-cbind(stepOneDB[,1:2],tm,tsd)
        ```


**We chose to use only the measurements on Body for mean and standard deviation, and make a Tidy Data set from those. This will allow us to use a more comprehensive variable name and hence improve on understand what each variable is measuring. Important properties of tidy data are: Each column is a variable, each row is an observation.**



        ```r
        ###Calculating the mean for every pair of subject/activity (item 5 of the requirement)

        tidyTable<-aggregate(fdf,by=list(fdf$id,fdf$activity),FUN=mean,na.rm=TRUE)

        ###Removing the extra columns created

        tidyTable<-tidyTable[,c(-1,-4)]

        ###Setting the names properly (item 3 of the requirement)

        names(tidyTable)<-c("Activity", "SubjectId", "AccelerometerMeanX","AccelerometerMeanY","AccelerometerMeanZ","AccelerometerSDX","AccelerometerSDY","AccelerometerSDZ")

        tidyTable$Activity<-factor(tidyTable$Activity)

        levels(tidyTable$Activity)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")


        write.table(tidyTable, file="tidyTable.txt", sep=",", row.names = FALSE)
        ```

**This will print the tidy data that was uploaded to the website.**
