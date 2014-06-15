# Reproducible Research: Peer Assessment 1

### After forking GitHub Repository needed to unzip file

Used the command "unzip("activity.zip")"

## Loading and preprocessing the data

### Load and display head and tail of data

```r
actdata <- read.csv("activity.csv")
head(actdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(actdata)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


### Check data types

```r
str(actdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


### Change date data from factor to date type

```r
actdata$date <- as.Date(actdata$date)
actdata$datetime <- paste(actdata[, 2], as.integer(actdata[, 3]/100), actdata[, 
    3] - 100 * as.integer(actdata[, 3]/100))
actdata$datetime <- strptime(actdata$datetime, "%Y-%m-%d %H %M")
str(actdata)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ datetime: POSIXlt, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
```


## What is mean total number of steps taken per day?

### Compute the Total number of steps taken each day


```r
stepsperday <- tapply(actdata$steps, actdata$date, sum, na.rm = TRUE)
hist(stepsperday)
```

![plot of chunk stepsperday](figure/stepsperday.png) 


###The mean and median are:

```r
paste("The mean is ", mean(stepsperday), " steps per day")
```

```
## [1] "The mean is  9354.22950819672  steps per day"
```

```r
paste("The median is ", median(stepsperday), "steps per day")
```

```
## [1] "The median is  10395 steps per day"
```

## What is the average daily activity pattern?

### Compute Total steps for each 5 min interval across days then average


```r
stepsperinterval <- aggregate(steps ~ interval, actdata, sum)
stepsperinterval$steps <- stepsperinterval$steps/length(unique(actdata$date))
tail(stepsperinterval)
```

```
##     interval  steps
## 283     2330 2.2623
## 284     2335 4.0820
## 285     2340 2.8689
## 286     2345 0.5574
## 287     2350 0.1967
## 288     2355 0.9344
```


### Plot average over days of steps per interval


```r
plot(stepsperinterval$interval, stepsperinterval$steps, type = "l")
```

![plot of chunk plotintervals](figure/plotintervals.png) 


### Compute Interval of max average steps


```r
stepsperinterval$interval[which.max(stepsperinterval$steps)]
```

```
## [1] 835
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
