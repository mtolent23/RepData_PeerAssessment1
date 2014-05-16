# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

(1) Load the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = NA)
```


(2) Process/transform the data into format suitable for analysis

```r
data$date <- as.Date(data$date)
```



## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

```r
clean_data = data[!is.na(data$steps), ]
```


(1) Make a histogram of the total number of steps taken each day

```r
par(las = 2)
steps.date <- aggregate(steps ~ date, data = clean_data, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, ylab = "Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


(2) Calculate and report the *mean* and *median* total number of steps taken per day

```r
mean(tapply(clean_data$steps, clean_data$date, sum))
```

```
## [1] 10766
```

```r
median(tapply(clean_data$steps, clean_data$date, sum))
```

```
## [1] 10765
```



## What is the average daily activity pattern?

(1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.interval <- aggregate(steps ~ interval, data = clean_data, FUN = mean)
plot(steps.interval, type = "l", xlab = "5-minute interval", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


(2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```



## Imputing missing values

(1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```


(2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

<b>I will replace the NA values with the means of the 5-minute intervals</b>

(3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
colnames(steps.interval) = c("interval", "avgsteps")
data <- merge(data, steps.interval, by.x = "interval", by.y = "interval")
data$steps = ifelse(is.na(data$steps), data$avgsteps, data$steps)
```


(4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
par(las = 2)
steps.date <- aggregate(steps ~ date, data = data, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, ylab = "Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10766
```


<b>The impact of imputing missing data on the estimates of the total daily number of steps is low</b>


## Are there differences in activity patterns between weekdays and weekends?

(1) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data$weekday = weekdays(data$date)
data$weekend = data$weekday == "Saturday" | data$weekday == "Sunday"
data$weekfactor = factor(data$weekend, levels = c("FALSE", "TRUE"), labels = c("weekday", 
    "weekend"))
```



(2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = data, subset = data$weekfactor == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

