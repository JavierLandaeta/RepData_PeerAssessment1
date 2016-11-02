#--------------------LOADING THE DATA----------------------------
#If you already download the zip file, please extract the files in your working directory
#and keep the names of file as "activity.csv".
#If you haven´t, the code will download the zip file and extract the files for you.

WD <- getwd()
dest_file1 <- paste(WD, "/activity.csv")
dest_file1 <- gsub(" /", "/", dest_file1)

#Checks if the files "activity.csv" exists in the WD
if (!file.exists(dest_file1, showWarnings = FALSE)){
  URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  temp <- tempfile()
  download.file(URL, temp)
  unzip(temp, c("activity.csv"), exdir = WD )
  ActivityData <- read.csv("activity.csv")
  unlink(temp)
}else{
  ActivityData <- read.csv("activity.csv")
}
#--------------------WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?----------------------------
# Calculate the total number of steps taken per day
library(ggplot2)
TotalSteps <- tapply(ActivityData$steps, ActivityData$date, FUN = sum, na.rm = TRUE)

#Make a histogram of the total number of steps taken each day
qplot(TotalSteps, binwidth = 1000, xlab = "Total number of steps taken each day")

#Calculate and report the mean and median of the total number of steps taken per day
mean(TotalSteps, na.rm = TRUE)
median(TotalSteps, na.rm = TRUE)

#-------------------WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN----------------------------
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Averages <- aggregate(x=list(steps = ActivityData$steps), by=list(interval = ActivityData$interval), FUN = mean, na.rm = TRUE)
ggplot(data = Averages, aes(x=interval, y=steps)) + geom_line() + xlab("5 minute interval") + ylab("average number of steps taken")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Averages[which.max(Averages$steps),]

#--------------------Imputing missing values----------------------------
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Missing <- is.na(ActivityData$steps)
table(Missing)

#Devise a strategy for filling in all of the missing values in the dataset. 
FillValue <- function(steps, interval) {
  Filled <- NA
  if (!is.na(steps))
    Filled <- c(steps)
  else
    Filled <- (Averages[Averages$interval==interval, "steps"])
  return(Filled)
}
FilledData <- ActivityData
FilledData$steps <- mapply(FillValue, FilledData$steps, FilledData$interval)

#Create a new dataset that is equal to the original dataset but with the missing data filled in
library(ggplot2)
TotalSteps <- tapply(ActivityData$steps, ActivityData$date, FUN = sum, na.rm = TRUE)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
qplot(TotalSteps, binwidth = 1000, xlab = "Total number of steps taken each day")
mean(TotalSteps, na.rm = TRUE)
median(TotalSteps, na.rm = TRUE)

#--------------------DIFFERENCES IN ACTIVITIES PATTERNS BETWEEN WEEKDAYS AND WEEKENDS----------------------------
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
WeekdayOrWeekend <- function(Date) {
  Day <- weekdays(Date)
  if (Day %in% c("lunes", "martes", "miércoles", "jueves", "viernes"))
    return("Weekday")
  else if (Day %in% c("sábado", "domingo"))
    return("Weekend")
  else
    stop("Invalid date")
}
FilledData$date <- as.Date(FilledData$date)
FilledData$day <- sapply(FilledData$date, FUN = WeekdayOrWeekend)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). 
Averages <- aggregate(steps ~ interval + day, data = FilledData, mean)
ggplot(Averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
 xlab("5-minute interval") + ylab("Number of steps")
