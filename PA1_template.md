---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading libraries needed for the project


```r
library(ggplot2)
```


## Loading and preprocessing the data

1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

```r
activity_data <- read.csv("activity.csv")
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity_data$date <- as.Date(as.character(activity_data$date),"%Y-%m-%d")
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
total_steps <- aggregate(steps~date,activity_data,sum,na.rm=TRUE)
```

2. Plot a Histogram of the total number of steps taken each day

```r
hist(total_steps$steps, main='Total Steps per Day', xlab='Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(activity_data$steps, na.rm=TRUE)
mean_steps
```

```
## [1] 37.3826
```

```r
median_steps <- median(activity_data$steps, na.rm=TRUE)
median_steps
```

```
## [1] 0
```

```r
summary(activity_data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval_steps <- aggregate(steps~interval,activity_data,mean,na.rm=TRUE)

plot(steps~interval,interval_steps,type='l',main='Average steps per interval',xlab='Interval',ylab='Average steps') 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval <- interval_steps$interval[interval_steps$steps==max(interval_steps$steps)]
max_interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing_steps <- sum(is.na(activity_data$steps))
missing_intervals <- sum(is.na(activity_data$interval))
missing_dates <- sum(is.na(activity_data$date))

missing_steps
```

```
## [1] 2304
```

```r
missing_intervals
```

```
## [1] 0
```

```r
missing_dates
```

```
## [1] 0
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
imputed_steps <- interval_steps$steps[match(activity_data$interval, interval_steps$interval)]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_data_imputed <- transform(activity_data, steps = ifelse(is.na(activity_data$steps), round(imputed_steps), activity_data$steps))

sum(is.na(activity_data_imputed$steps))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_imputed <- aggregate(steps~date,activity_data_imputed,sum)

hist(total_steps_imputed$steps, main='Total Steps per Day', xlab='Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
mean_steps_imputed <- mean(activity_data_imputed$steps)
mean_steps_imputed
```

```
## [1] 37.38069
```

```r
median_steps_imputed <- median(activity_data_imputed$steps)
median_steps_imputed
```

```
## [1] 0
```

```r
summary(activity_data_imputed$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   27.00  806.00
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_data_imputed$day_type <- ifelse(weekdays(activity_data_imputed$date) %in% c('Monday','Tuesday','Wednesday','Thursday','Friday'), 'Weekday', 'Weekend') 

str(activity_data_imputed)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  2 0 0 0 0 2 1 1 0 1 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day_type: chr  "Weekday" "Weekday" "Weekday" "Weekday" ...
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
day_type_steps <- aggregate(steps~interval+day_type,activity_data_imputed,mean)

ggplot(day_type_steps, aes(interval, steps, color=day_type)) +
    geom_line() +
    labs(title = "Average daily steps by type of day", x = "Interval", y = "Average number of steps") +
    facet_wrap(~day_type, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
