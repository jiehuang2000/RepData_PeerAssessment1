# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("~/Desktop/Reproducible_Research/PA1")
df <- read.csv("activity.csv")
```


Have a look at the first 5 lines of the data

```r
head(df)
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


## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day

```r
day_sum <- tapply(df$steps, df$date, sum, na.rm = TRUE)
hist(day_sum, breaks = 10, main = "The histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


calculate the mean and median total number of steps taken per day

```r
mean(day_sum)
```

```
## [1] 9354
```

```r
median(day_sum)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
five_minute_mean <- tapply(df$step, df$interval, mean, na.rm = TRUE)
plot(df$interval[1:288], five_minute_mean, type = "l", xlab = "5 minutes interval in one day", 
    ylab = "average number of steps taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- max(five_minute_mean)
index <- match(max, five_minute_mean)
max_point <- five_minute_mean[index]
max_interval <- as.numeric(names(max_point))
max_interval
```

```
## [1] 835
```

So, the 835th 5-minute interval contains the maxinum number of steps.
## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing_values <- is.na(df[, 1])
number_NA <- sum(missing_values)
number_NA
```

```
## [1] 2304
```

So, the total missing value number is 2304

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I choose to fill the NAs with the mean for that 5-minute interval:

```r
five_minute_mean <- tapply(df$step, df$interval, mean, na.rm = TRUE)
df[missing_values, 1] <- five_minute_mean[df[missing_values, 3]%/%100 * 12 + 
    (df[missing_values, 3]%%100)/5 + 1]

head(five_minute_mean)
```

```
##       0       5      10      15      20      25 
## 1.71698 0.33962 0.13208 0.15094 0.07547 2.09434
```

```r
head(df)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```


Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df2 <- df
missing_values2 <- is.na(df2[, 1])
number_NA2 <- sum(missing_values2)
number_NA2
```

```
## [1] 0
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  

```r
day_sum2 <- tapply(df2$steps, df2$date, sum)
hist(day_sum2, breaks = 10, main = "The histogram of the total number of steps taken each day (NA filled)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r

mean(day_sum2)
```

```
## [1] 10766
```

```r
median(day_sum2)
```

```
## [1] 10766
```

Q: Do these values differ from the estimates from the first part of the assignment?  
A: So the histogram, mean and median do differ from the first part when NAs were not filled.  
Q: What is the impact of imputing missing data on the estimates of the total daily number of steps?  
A: Making the data less biased.   



## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
df2$date = as.Date(df2$date)
df2$day <- weekdays(df2$date) == "Sunday" | weekdays(df2$date) == "Saturday"
head(df2)
```

```
##     steps       date interval   day
## 1 1.71698 2012-10-01        0 FALSE
## 2 0.33962 2012-10-01        5 FALSE
## 3 0.13208 2012-10-01       10 FALSE
## 4 0.15094 2012-10-01       15 FALSE
## 5 0.07547 2012-10-01       20 FALSE
## 6 2.09434 2012-10-01       25 FALSE
```

```r

df2_weekday <- df2[df2$day == FALSE, ]
df2_weekend <- df2[df2$day == TRUE, ]
```



Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
five_minute_mean_weekday <- tapply(df2_weekday$step, df2_weekday$interval, mean)
five_minute_mean_weekend <- tapply(df2_weekend$step, df2_weekend$interval, mean)
par(mfrow = c(2, 1))
plot(df$interval[1:288], five_minute_mean_weekday, type = "l", xlab = "", ylab = "number of steps", 
    main = "weekdays")
plot(df$interval[1:288], five_minute_mean_weekend, type = "l", xlab = "interval", 
    ylab = "", main = "weekends")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


