# Reproducible Research: Peer Assessment 1



### Loading and preprocessing the data


```r
activity.data <- read.csv("activity.csv", stringsAsFactors=F)
```

### What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day

```r
# for each factor (ie date), sum steps, ignore any NAs
total.nr.of.steps.per.day <- aggregate(activity.data$steps, list(date = activity.data$date), sum, na.rm=T)$x
hist(total.nr.of.steps.per.day, xlab="total number of steps per day", main="histogram of total number of steps per day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

mean number of steps taken per day

```r
mean(total.nr.of.steps.per.day, na.rm=T)
```

```
## [1] 9354.23
```

median number of steps taken per day

```r
median(total.nr.of.steps.per.day, na.rm=T)
```

```
## [1] 10395
```

### What is the average daily activity pattern?

```r
#for each factor (ie interval(288)), report average of steps
average.steps.across.all.days <- aggregate(activity.data$steps, list(interval = activity.data$interval), mean, na.rm=T)$x

plot(unique(activity.data$interval), average.steps.across.all.days, type="l", xlab="5-minute intervals", ylab="average number of steps taken, averaged across all days")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
The following 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps

```r
unique(activity.data$interval)[which (average.steps.across.all.days==max(average.steps.across.all.days))]
```

```
## [1] 835
```

### Imputing missing values

the total number of missing values in the dataset

```r
nrow(activity.data[(is.na(activity.data$steps)),])
```

```
## [1] 2304
```

fill in all of the missing values in the dataset by using
the mean for that 5-minute interval, etc.

```r
# keep a copy of the original dataset before imputing the missing values
original.activity.data <- activity.data
activity.data$interval <- as.factor(activity.data$interval)
missing.imputed.values <- average.steps.across.all.days[activity.data[(is.na(activity.data$steps)),]$interval]
activity.data[(is.na(activity.data$steps)),]$steps <- missing.imputed.values
```

Make a histogram of the total number of steps taken each day

```r
# for each factor (ie date), sum steps, na.rm=T
total.nr.of.steps.per.day <- aggregate(activity.data$steps, list(date = activity.data$date), sum, na.rm=T)$x
hist(total.nr.of.steps.per.day, xlab="total number of steps per day", main="histogram of total number of steps per day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

mean number of steps taken per day

```r
mean(total.nr.of.steps.per.day, na.rm=T)
```

```
## [1] 10766.19
```

median number of steps taken per day

```r
median(total.nr.of.steps.per.day, na.rm=T)
```

```
## [1] 10766.19
```

The values for the mean and median differ from the estimates from the first part of the assignment.
By imputing missing data on the estimates of the total daily number of steps, the mean and median values converge to the same value.


```r
activity.data$weektime <- weekdays(as.Date(activity.data$date))
activity.data[(activity.data$weektime %in% c("Saturday", "Sunday")),]$weektime <- "weekend"
activity.data[(!(activity.data$weektime %in% c("weekend"))),]$weektime <- "weekday"

activity.data$weektime <- as.factor(activity.data$weektime)
```

### Are there differences in activity patterns between weekdays and weekends?


```r
#5-minute interval (x-axis) and the average number of steps taken, averaged
#across all weekday days or weekend days (y-axis).
#for each factor (ie interval(288)), for each factor of weektime, report average of #steps
# for each interval factor, for each weektime factor, average steps,

aggregate.data <-aggregate(activity.data$steps, list(interval = activity.data$interval, weektime = activity.data$weektime), mean, na.rm=T)

library(lattice)
xyplot(x~interval|weektime, data = aggregate.data, type = "l", layout=c(1,2), xlab="interval", ylab="number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

```r
# REGENERATE X-AXIS
```

