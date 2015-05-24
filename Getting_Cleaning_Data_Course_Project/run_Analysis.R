## run_Analysis.R

getMeasurements <- function(sourcedir, type) {
   
    
    # Load Subject file
    subjectDF <- read.table(paste(sourcedir,"/",type,"/subject_",type,".txt",sep = ""))
    names(subjectDF) <- "SubjectId"
    
    # Load activity file
    activityDF <- read.table(paste(sourcedir,"/",type,"/y_",type,".txt",sep = ""))
    names(activityDF) <- "ActivityId"   
    
    # Load Measurements file
    measurementDF <- read.table(paste(sourcedir,"/",type,"/X_",type,".txt",sep = ""))
    # names(measurementDF) <- measurementLabels[,2]
    names(measurementDF) <- measurementLabels
    
    measurementDF <- cbind(subjectDF,activityDF,measurementDF)
    # measurementDF <- cbind(measurementDF,measurement)
    
    
    
    # Get labels that are "mean" and "std"
    subMeasurementLabels <- c("SubjectId","ActivityId",as.character(measurementLabels[grepl("Mean",measurementLabels) |grepl("StdDev",measurementLabels)]))
    
    return(measurementDF[,subMeasurementLabels])
}

sourcedir <- "C:/Data Science/workspace/cleandata/project/UCI HAR Dataset"

# Load Training Measurement labels
measurementLabels <- read.table(paste(sourcedir,"/features.txt",sep = ""))
measurementLabels <- as.character(measurementLabels[,2])

#Standardize Measurement lables
for (i in 1:length(measurementLabels)){
    measurementLabels[i] = gsub("\\()","",measurementLabels[i])
    measurementLabels[i] = gsub("-std","StdDev",measurementLabels[i])
    measurementLabels[i] = gsub("-mean","Mean",measurementLabels[i])
    measurementLabels[i] = gsub("^(t)","Time",measurementLabels[i])
    measurementLabels[i] = gsub("^(f)","Frequency",measurementLabels[i])
#     colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
#     colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
#     colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
#     colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
#     colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
#     colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
#     colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Std and Mean Labels
stdAndMeanLabels <- as.character(measurementLabels[grepl("Mean",measurementLabels) |grepl("StdDev",measurementLabels)])
                                 
# Load Activity labels
activityLabels <- read.table(paste(sourcedir,"/activity_labels.txt",sep = ""))
names(activityLabels) <- c("ActivityId","ActivityName")

## Get test measurements
testMeasurementDF <- getMeasurements(sourcedir,"test")
print(nrow(testMeasurementDF))

## Get train measurements
trainMeasurementDF <- getMeasurements(sourcedir,"train")
print(nrow(trainMeasurementDF))

## Combine the two measurements
totalMeasurementDF <- rbind(testMeasurementDF,trainMeasurementDF)
print(nrow(totalMeasurementDF))

## Create a Tidy data frame by summarizing the totalMeasurementDF table to include just the mean of each variable for each subject and activity
tidyDF = aggregate(totalMeasurementDF[,names(totalMeasurementDF) != c("SubjectId","ActivityId")],
                      by=list(SubjectId = totalMeasurementDF$SubjectId,ActivityId=totalMeasurementDF$ActivityId),mean)
print(head(tidyDF))

# Merging the Tidy Data Frame with Activity Labels to include Activity NAmes
tidyDF = merge(tidyDF,activityLabels,by='ActivityId',all.x=TRUE);

# Rearranging columns
tidyDF <- cbind(tidyDF[,c("SubjectId","ActivityId","ActivityName")],tidyDF[stdAndMeanLabels])

# Write the Tidy Data Frame to a file
write.table(tidyDF,paste(sourcedir,"/MeasurementData.txt",sep = ""),row.names=FALSE,sep=" ")


