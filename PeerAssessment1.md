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
paste("The interval with the max steps is ", stepsperinterval$interval[which.max(stepsperinterval$steps)])
```

```
## [1] "The interval with the max steps is  835"
```


## Imputing missing values

###Get Total number of NA values in steps

```r
paste("The Total number if NA Values is ", sum(is.na(actdata$steps)))
```

```
## [1] "The Total number if NA Values is  2304"
```


### Strategy for imputing missing values

How are NA values distributed?

```r
nabydates <- tapply(is.na(actdata$steps), actdata$date, sum)
nabydates
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0        288          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0        288          0          0        288          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0        288        288          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0        288          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##        288
```


Since all NA are confined to single dates and all values are missing from each of those dates it might make sense to use the mean from each interval over all the other dates
The mean should be taken though over just the dates without NA values


```r
stepsperinterval2 <- aggregate(steps ~ interval, actdata, sum)
stepsperinterval2$steps <- stepsperinterval2$steps/length(nabydates[nabydates == 
    0])
tail(stepsperinterval2)
```

```
##     interval  steps
## 283     2330 2.6038
## 284     2335 4.6981
## 285     2340 3.3019
## 286     2345 0.6415
## 287     2350 0.2264
## 288     2355 1.0755
```




```r
nactdata <- actdata
sum(is.na(nactdata$steps))
```

```
## [1] 2304
```

```r
nabydates <- tapply(is.na(actdata$steps), actdata$date, sum)
for (i in names(nabydates[nabydates != 0])) nactdata[nactdata$date == i, "steps"] <- stepsperinterval2$steps
head(nactdata)
```

```
##     steps       date interval            datetime
## 1 1.71698 2012-10-01        0 2012-10-01 00:00:00
## 2 0.33962 2012-10-01        5 2012-10-01 00:05:00
## 3 0.13208 2012-10-01       10 2012-10-01 00:10:00
## 4 0.15094 2012-10-01       15 2012-10-01 00:15:00
## 5 0.07547 2012-10-01       20 2012-10-01 00:20:00
## 6 2.09434 2012-10-01       25 2012-10-01 00:25:00
```

```r
tail(nactdata)
```

```
##        steps       date interval            datetime
## 17563 2.6038 2012-11-30     2330 2012-11-30 23:30:00
## 17564 4.6981 2012-11-30     2335 2012-11-30 23:35:00
## 17565 3.3019 2012-11-30     2340 2012-11-30 23:40:00
## 17566 0.6415 2012-11-30     2345 2012-11-30 23:45:00
## 17567 0.2264 2012-11-30     2350 2012-11-30 23:50:00
## 17568 1.0755 2012-11-30     2355 2012-11-30 23:55:00
```


### recompute means with new data frame

```r
stepsperinterval <- aggregate(steps ~ interval, nactdata, sum)
stepsperinterval$steps <- stepsperinterval$steps/length(unique(nactdata$date))
tail(stepsperinterval)
```

```
##     interval  steps
## 283     2330 2.6038
## 284     2335 4.6981
## 285     2340 3.3019
## 286     2345 0.6415
## 287     2350 0.2264
## 288     2355 1.0755
```


The new means are different from the original computations but are consistent with the means of the old data adjusting the divisor for only the days with data
The total steps per day is not impacted since NA values were ignored for those totals

```r
nstepsperday <- tapply(actdata$steps, nactdata$date, sum, na.rm = TRUE)
hist(nstepsperday)
```

![plot of chunk nstepsperday](figure/nstepsperday.png) 



## Are there differences in activity patterns between weekdays and weekends?

Add weekday

```r
day <- weekdays(as.Date(nactdata$date))
day[!(day %in% c("Saturday", "Sunday"))] <- "weekday"
day[day %in% c("Saturday", "Sunday")] <- "weekend"
nactdata$weekday <- as.factor(day)
splitdata <- split(nactdata, nactdata$weekday)
stepsperinterval <- aggregate(steps ~ interval, splitdata[["weekend"]], sum)
stepsperinterval$steps <- stepsperinterval$steps/length(unique(splitdata[["weekend"]]$date))
par(mfcol = c(2, 1))
plot(stepsperinterval$interval, stepsperinterval$steps, type = "l", main = "weekend")
stepsperinterval <- aggregate(steps ~ interval, splitdata[["weekday"]], sum)
stepsperinterval$steps <- stepsperinterval$steps/length(unique(splitdata[["weekday"]]$date))
plot(stepsperinterval$interval, stepsperinterval$steps, type = "l", main = "weekday")
```

![plot of chunk weekday](figure/weekday.png) 



