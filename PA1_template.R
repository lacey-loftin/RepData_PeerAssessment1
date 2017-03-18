##"Peer-graded Assignment: Course Project 1"

  #### Author: Lacey Glencora Loftin 
  
## Loading and preprocessing the data:

data <- read.csv("activity.csv")

## Load necessary packages
library(ggplot2)

## Histogram of the total number of steps taken each day:
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm = TRUE)
hist(total.steps, main="Histogram of total number of steps per day", xlab="Total number of steps in a day") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))

## What is mean and median number of steps taken per day? 
mean(total.steps, na.rm = TRUE)
median(total.steps, na.rm = TRUE)


## Time series plot of the average number of steps taken: What is the average daily activity pattern?
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averages, aes(x=interval, y=steps)) + 
    geom_line() + xlab("5-Minute Interval") + 
    ylab("Average Nnumber of Steps Taken") + 
    ggtitle("Average Number of Steps Taken All Day") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))

## On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?
averages[which.max(averages$steps),]

## Calculating the data
missing <- is.na(data$steps)

#### How many missing
table(missing)

## Imputting the missing values

#### Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

## Histogram of the total number of steps taken each day after missing values are imputed.

#### the new data set
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
#### histogram
hist(total.steps, xlab="Total Number Steps Taken Each Day", main = "Total Number of Steps Taken Each Day") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))

## the mean and median of the new dataset
mean(total.steps)
median(total.steps)


## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends: Are there differences in activity patterns between weekdays and weekends?
#### First, seperate out the weekday for each measurement in the dataset. In
#### this part, we use the new dataset with the filled-in values.


weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


#### Now make a panel plot containing plots of average number of steps taken
#### on weekdays vs weekends.

averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps") + ggtitle("Weekdays VS Weekends") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


