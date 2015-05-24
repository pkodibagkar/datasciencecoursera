## run_Analysis.R



### Step 1. Initialize common variables
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
}

# Std and Mean Labels
stdAndMeanLabels <- as.character(measurementLabels[grepl("Mean",measurementLabels) |grepl("StdDev",measurementLabels)])
                                 
# Load Activity labels
activityLabels <- read.table(paste(sourcedir,"/activity_labels.txt",sep = ""))
names(activityLabels) <- c("ActivityId","ActivityName")

### Step 2. Create a data frame after merging subject, activity and measurement data files

getMeasurements <- function(sourcedir, type) {
    # Load Subject file
    subjectDF <- read.table(paste(sourcedir,"/",type,"/subject_",type,".txt",sep = ""))
    names(subjectDF) <- "SubjectId"
    
    # Load activity file
    activityDF <- read.table(paste(sourcedir,"/",type,"/y_",type,".txt",sep = ""))
    names(activityDF) <- "ActivityId"   
    
    # Load Measurements file
    measurementDF <- read.table(paste(sourcedir,"/",type,"/X_",type,".txt",sep = ""))
    names(measurementDF) <- measurementLabels
    
    measurementDF <- cbind(subjectDF,activityDF,measurementDF)
    
    # Get labels that are "mean" and "std"
    subMeasurementLabels <- c("SubjectId","ActivityId",as.character(measurementLabels[grepl("Mean",measurementLabels) |grepl("StdDev",measurementLabels)]))
    
    return(measurementDF[,subMeasurementLabels])
}

### Step 3. Create a merged data set

## Get test measurements
testMeasurementDF <- getMeasurements(sourcedir,"test")
# print(nrow(testMeasurementDF))

## Get train measurements
trainMeasurementDF <- getMeasurements(sourcedir,"train")
# print(nrow(trainMeasurementDF))

## Combine the two measurements
totalMeasurementDF <- rbind(testMeasurementDF,trainMeasurementDF)
# print(nrow(totalMeasurementDF))

### Step 4. Create a Tidy data frame by summarizing the totalMeasurementDF table to include just the mean of each variable for each subject and activity
tidyDF = aggregate(totalMeasurementDF[,names(totalMeasurementDF) != c("SubjectId","ActivityId")],
                      by=list(SubjectId = totalMeasurementDF$SubjectId,ActivityId=totalMeasurementDF$ActivityId),mean)
# print(head(tidyDF))

# Merging the Tidy Data Frame with Activity Labels to include Activity NAmes
tidyDF = merge(tidyDF,activityLabels,by='ActivityId',all.x=TRUE);

# Rearranging columns
tidyDF <- cbind(tidyDF[,c("SubjectId","ActivityId","ActivityName")],tidyDF[stdAndMeanLabels])

### Step 5. Write the Tidy Data Frame to a file
write.table(tidyDF,paste(sourcedir,"/MeasurementData.txt",sep = ""),row.names=FALSE,sep=" ")


