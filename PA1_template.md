---
title: "Reproducible Research: Peer Assessment 1"
author: "Xiao"
date: "12/28/2020"
output: 
  html_document:
    keep_md: true
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But this data remains under-utilized because the raw data is hard to obtain and there are limited tools and statistical methods available for interpreting the data. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data
The data for this assignment can be downloaded from the course web site: Dataset: Activity monitoring data [52K] The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA) \
* date: The date on which the measurement was taken in YYYY-MM-DD format \
* interval: Identifier for the 5-minute interval in which measurement was taken \

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment (https://github.com/rdpeng/RepData_PeerAssessment1). You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

```r
if (!file.exists('activity.csv')) {
  unzip('activity.zip',overwrite=TRUE)
}
activity <- read.csv("activity.csv", header = T, sep = ",")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis \
No need to transform the data. 

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
total_of_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_of_steps_per_day
```

```
## [1] 570608
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
#head(total_steps_each_day)

## Generating the histogram by each day
library(ggplot2)
ggplot(total_steps_each_day, aes(x = steps)) +
    geom_histogram(fill = "grey", binwidth = 1000) +
    ylim(0, 15) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_mean
```

```
## [1] 10766.19
```

```r
total_steps_each_day_median <- median(total_steps_each_day$steps)
total_steps_each_day_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
five_minutes_average <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
ggplot(five_minutes_average, aes(x = interval , y = steps)) + 
  geom_line(color="grey", size = 1) + 
  labs(title = "Average Daily Steps", x = "Interval", y = "Average number of steps taken per day") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps <- max(five_minutes_average$steps)
for (i in 1:288) 
{
    if (five_minutes_average$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
}
five_minute_interval_at_max_steps 
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(activity[is.na(activity),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Impute the data using the mean for that 5-minute interval
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(five_minutes_average$steps[match(activity$interval, five_minutes_average$interval)], 0), activity$steps)
head(activity, 5)
```

```
##   steps       date interval CompleteSteps
## 1    NA 2012-10-01        0             2
## 2    NA 2012-10-01        5             0
## 3    NA 2012-10-01       10             0
## 4    NA 2012-10-01       15             0
## 5    NA 2012-10-01       20             0
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityFull <- data.frame(steps = activity$CompleteSteps, date = activity$date, interval = activity$interval)
head(activityFull, n=5)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day_rmna <- aggregate(steps ~ date, data = activityFull, FUN = sum, na.rm = TRUE)
#head(total_steps_each_day)

## Generating the histogram by each day
ggplot(total_steps_each_day_rmna, aes(x = steps)) +
    geom_histogram(fill = "grey", binwidth = 1000) +
    ylim(0, 15) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
mean(total_steps_each_day_rmna$steps)
```

```
## [1] 10765.64
```

```r
median(total_steps_each_day_rmna$steps)
```

```
## [1] 10762
```
The mean and median have a small variance between the original data and the data with filled NA using the mean for that 5-minute interval.

The impact of the missing data has the biggest effect on the 10000 - 150000 step intervals. Most of missing data are in the 10000 - 150000 step intervals. 

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
# Create variable with date in correct format
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday == 'Saturday' | activityFull$weekday == 'Sunday', 'weekend', 'weekday')
# see first 10 values
activityFull$DayType = as.factor(activityFull$DayType)
head(activityFull, n = 5)
```

```
##   steps       date interval   RealDate weekday DayType
## 1     2 2012-10-01        0 2012-10-01  Monday weekday
## 2     0 2012-10-01        5 2012-10-01  Monday weekday
## 3     0 2012-10-01       10 2012-10-01  Monday weekday
## 4     0 2012-10-01       15 2012-10-01  Monday weekday
## 5     0 2012-10-01       20 2012-10-01  Monday weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
five_minutes_average2 <- aggregate(steps ~ interval + DayType, data = activityFull, FUN = mean)
ggplot(five_minutes_average2, aes(x = interval , y = steps)) + 
  geom_line(color="grey", size = 1) + 
  facet_grid(DayType  ~ .) +
  labs(title = "Average Daily Steps", x = "Interval", y = "Average number of steps taken per day") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
