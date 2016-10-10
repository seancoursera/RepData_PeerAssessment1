---
title: "Course Project: Reproducible Research"
output: html_document
---

#Load the data to activity, convert the date to act.

```{r echo = TRUE}
activity <- read.csv('activity.csv')
activity1 <- activity[complete.cases(activity),]
act <- activity1
act$date <- as.POSIXct(strptime(activity1$date, '%Y-%m-%d'))
```

##Load some packages like dplyr, ggplot2 and psych.

```{r echo = TRUE}
library(dplyr)
library(ggplot2)
library(psych)
```

#What is mean total number of steps taken per day?

1. Calculate total number of steps taken per day.

```{r echo = TRUE}
(totalsteps <- summarise(group_by(activity1, date), sum(steps), mean(steps)))
```

2. Make a histogram of the total number of steps taken each day

```{r echo = TRUE}
hist(totalsteps$`sum(steps)`, main = 'Total steps per day', xlab = 'Total steps', col = 'pink')

```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r echo = TRUE}
(meanstep <- mean(totalsteps$`sum(steps)`))
(medianstep <- median(totalsteps$`sum(steps)`))

```


#What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
```{r echo = TRUE}
timeseries <- summarise(group_by(activity1, interval), mean(steps))

plot(timeseries$interval, timeseries$`mean(steps)`, type = 'l', 
     main = '5-minute interval and average steps', xlab = 'interval', ylab = 'average steps')

```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo = TRUE}
act[which.max(act$steps),]

```


#Imputing missing values

1. Calculate and report the total number of missing values in the dataset 
```{r echo = TRUE}
summary(is.na(activity))
summary(activity)

```

2.filling in all of the missing values with 'mean(steps)' and create new dataset newact , repeat meansteps 61 times and fill in them to original dataset$steps in which steps is NA

```{r echo = TRUE}
newsteps <- (rep(summarise(group_by(activity1, interval), mean(steps))$'mean(steps)', times = 61))

newact <- activity
newact$steps[is.na(newact$steps)] <- newsteps[is.na(newact$steps)]


(totalsteps2 <- summarise(group_by(newact, date), sum(steps), mean(steps)))


```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
```{r echo = TRUE}
hist(totalsteps2$`sum(steps)`, main = 'Total steps per day', xlab = 'Total steps', col = 'pink')

```


```{r echo = TRUE}
(meanstep <- mean(totalsteps2$`sum(steps)`))
(medianstep <- median(totalsteps2$`sum(steps)`))

```

###Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###mean and median are almost the same as the first part of the assignment, median increased by 1. 

#Are there differences in activity patterns between weekdays and weekends? 
 
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r echo = TRUE}
newact$date <- as.POSIXct(strptime(newact$date, '%Y-%m-%d'))

newact$day <- weekdays(newact$date)

newact$day_type <- c('weekday')

for (i in 1:nrow(newact)){
    if (newact$day[i] == 'Saturday' || newact$day[i] == 'Sunday'){
        newact$day_type[i] <- 'weekend'
    }
}

newact$day_type <- as.factor(newact$day_type)

interval_steps <- aggregate(steps~interval+day_type,newact,mean)

```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r echo = TRUE}
qplot(interval, steps, data=interval_steps, geom=c("line"), xlab="Interval",
      ylab="Number of steps") + facet_wrap(~day_type, ncol=1)

```

