#Load packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

#data.table   reshape2
#TRUE       TRUE

#get the path
path <- getwd()
#C:/Users/Sasha/Documents/GitHub/GettingAndCleaningData/Project"

#get the data
url <-
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {
  dir.create(path)
}
download.file(url, file.path(path, f))

#unzip the file
executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
parameters <- "x"
cmd <-
  paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path,
                                                                           f), "\""))
system(cmd)
#The folder created is called UCI HAR Dataset
#change the path to this folder to check the files
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

#read the subject files
dtSubjectTrain <-
  read.table(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <-
  read.table(file.path(pathIn, "test", "subject_test.txt"))

#read the activity file:
dtActivityTrain <-
  read.table(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest <-
  read.table(file.path(pathIn, "test", "Y_test.txt"))

#read the train/test sets:

dtTrain <- read.table(file.path(pathIn, "train", "X_train.txt"))
dtTest <- read.table(file.path(pathIn, "test", "X_test.txt"))


#convert the resulting data frames to data tables:
dtSubjectTrain <- data.table(dtSubjectTrain)
dtSubjectTest <- data.table(dtSubjectTest)
dtActivityTrain <- data.table(dtActivityTrain)
dtActivityTest <- data.table(dtActivityTest)
dtTrain <- data.table(dtTrain)
dtTest <- data.table(dtTest)

# 1. Merge the training and the test sets to create one data set.
#concatenate the data tables
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

#merge the columns
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

#set key
setkey(dt, subject, activityNum)

#2.Extract only the measurements on the mean and standard deviation for each measurement.
#get the features information from the file

dtFeatures <- read.table(file.path(pathIn, "features.txt"))
dtFeatures <- data.table(dtFeatures)

#change the names of the columns to be human readable

setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

#only get the mean and standard deviation measurements

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

#add a column to match the the names of the columns in dt

dtFeatures$featureCode <- dtFeatures[,paste0("V", featureNum)]
head(dtFeatures)
#dtFeatures$featureCode

#subset the variables using the names

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[,select, with = FALSE]

#3.Use descriptive activity names to name the activities in the data set
#read the activity_labels.txt file which contains the descriptive names for the activities
dtActivityNames <-
  read.table(file.path(pathIn, "activity_labels.txt"))
dtActivityNames <- data.table(dtActivityNames)
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
head(dtActivityNames)

#4.Appropriately label the data set with descriptive variable names.
#merge the activity labels
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
#add the ActivityName as a key
setkey(dt, subject, activityNum, activityName)
#melt the dt to reshape it to a tall and narrow format
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
dt <-
  merge(dt, dtFeatures[,list(featureNum, featureCode, featureName)], by =
          "featureCode", all.x = TRUE)
#activityName and featureName are factors in dt
#we set the keys for dt to use further in the split
setkey(dt, subject, activityName, featureName)
#create the tidy dt by the keys defined
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

#save the tidy data table into a file
f <- file.path(path, "TidyDataSet.txt")
write.table(dtTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)
