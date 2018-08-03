Reproducible Research - Course Project 1
========================================

## Evaluation of Activity Monitoring Data ##

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals throughout the day. The data 
consists of two months of data from an anonymous individual collected during 
the months of October and November, 2012 and includes the number of steps taken 
in 5 minute intervals each day.

First load required libraries
```{r libraries, echo=TRUE}
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(scales)
```

### Read in *activity.csv* file ###
Check if the *activity.csv* file exists, if it doesn't, unzip the 
*repdata_data_activity.zip* file, then read the *activity.csv* file.

```{r readinfile, echo=TRUE}
if(!file.exists("activity.csv")) {
    unzip("repdata_data_activity.zip")
}
activityDataNA <- read_csv("activity.csv")
head(activityDataNA)
```

### Calculate the total number of steps taken per day. ###

The "steps" column contains over 2300 missing (NA) values, with the date and 
interval columns containing no missing values. The following code removes these
missing values from the data frame before calculating the total steps per day.

```{r totalsteps, echo=TRUE}
activityData <- activityDataNA %>% drop_na()
totalStepsDay <- aggregate(steps ~ date, activityData, sum)
head(totalStepsDay)
g <- ggplot(totalStepsDay, aes(totalStepsDay$steps)) + geom_histogram(fill = "red", bins = 53) + labs(title = "Daily Mean Steps Taken", x = "Total Steps per Day", y = "Frequency of Occurence")
print(g)
```

### Calculate and report the mean and median of the total number of steps ###
### taken per day. ###

The overall mean and median steps are first calculated and displayed then the 
individual mean and median (over the 53 days) are calculated and displayed

```{r meanmedian, echo=TRUE}
overallMean <- mean(activityData$steps)
overallMedian <- median(activityData$steps)
activityDataGroup <- group_by(activityData, date)
activityDataperDay <- summarise(activityDataGroup, count = n(), 
                                meanSteps = mean(steps), 
                                medianSteps = median(steps))
```

Overall Mean is: `r overallMean`
Overall Median is: `r overallMedian`

Mean and median grouped by day are:
```{r meanmediantable, echo=TRUE}
print.data.frame(activityDataperDay)
```

### Make a time series plot of the 5-minute interval (x-axis) and the ###
### average number of steps taken, averaged across all days (y-axis). ###

The following r code creates a time series plot

```{r activityPattern, echo=TRUE}
stepsPer5mInterval <- aggregate(steps ~ interval, activityData, mean)
g <- ggplot(stepsPer5mInterval, aes(interval, steps)) + geom_line() + 
    labs(title = "Average Daily Activity Pattern", x = "5-Minute Intervals", 
         y = "Average Number of Steps")
print(g)
```

### Which 5-minute interval, on average across all the days in the dataset, ### 
### contains the maximum number of steps? ###

```{r fiveminuteinterval, echo=TRUE}
FiveminInt <- stepsPer5mInterval$interval[which.max(stepsPer5mInterval$steps)]
```

The maximum number of steps is found within the interval: `r FiveminInt`

### Calculate and report the total number of missing values in the dataset ###
### (i.e. the total number of rows with NA's). ###

```{r countnas, echo=TRUE}
naCalc <- map(activityDataNA, ~sum(is.na(.)))
```

Total number of missing values (i.e. NA's): `r naCalc`

### Devise a strategy for filling in all of the missing values in the dataset, ###
### then, create a new dataset that is equal to the original dataset but with ###
### the missing data filled in. ###

Using the *impute()* function, missing values are filled-in with the mean

```{r imputemissingdata, echo=TRUE}
activityDataNew <- activityDataNA
activityDataNew$steps <- impute(activityDataNA$steps, mean)
```

### Make a histogram of the total number of steps taken each day and Calculate ### 
### and report the mean and median total number of steps taken per day. ###

```{r imputemissingdataplot, echo=TRUE}
totStepsPerDay <- aggregate(steps ~ date, activityDataNew, sum)
g <- ggplot(totStepsPerDay, aes(totStepsPerDay$steps)) + 
    geom_histogram(fill = "red", bins = 61) + 
    labs(title = "Total Steps Taken", x = "Total Steps per Day", 
         y = "Frequency of Occurence")
print(g)
```

Do these values differ from the estimates from the first part of the assignment? 

**Yes.**

What is the impact of imputing missing data on the estimates of the total daily 
number of steps?

** There is a higher frequency of occurence around 11,000 steps per day for the **
** new data table where NA's were replaced with mean values **

```{r meanmedianupdate, echo=TRUE}
overallMean2 <- mean(totStepsPerDay$steps)
overallMedian2 <- median(totStepsPerDay$steps)
activityDataGroup2 <- group_by(totStepsPerDay, date)
activityDataperDay2 <- summarise(activityDataGroup2, count = n(), 
                                meanSteps = mean(steps), 
                                medianSteps = median(steps))
```

New Overall Mean is: `r overallMean2`
New Overall Median is: `r overallMedian2`

New Mean and median grouped by day are:
```{r meanmediantablenew, echo=TRUE}
print.data.frame(activityDataperDay2)
```

###Are there differences in activity patterns between weekdays and weekends?###

*(Using the dataset with the filled-in missing values)*

Create a new factor variable in the dataset with two levels – “weekday” and 
“weekend” indicating whether a given date is a weekday or weekend day

```{r convertdatetype, echo=TRUE}
activityDataNew$dateType <- ifelse(as.POSIXlt(activityDataNew$date)$wday %in% 
                                        c(0,6), 'weekend', 'weekday')
```

### Make a panel plot containing a time series plot of the 5-minute interval ###
### (x-axis) and the average number of steps taken, averaged across all ###
### weekday days or weekend days (y-axis). ###

```{r timeseriesplot, echo=TRUE}
averagedactivityDataNew <- aggregate(steps ~ interval + dateType, 
                                     activityDataNew, mean)
g <- ggplot(averagedactivityDataNew, aes(interval, steps)) + geom_line() + 
    facet_grid(dateType ~ .) + 
    labs(title = "Time Series across Weekends and Weekdays", 
         x = "5-Minute Intervals", y = "Average Number of Steps")
print(g)
```

