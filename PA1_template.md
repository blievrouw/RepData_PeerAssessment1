---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(dplyr)
options(scipen=999)
```

## Loading and preprocessing the data

```r
activity_df <- read.csv(unz("activity.zip", "activity.csv"), header = T, na.strings = "NA")
activity_df$date <- as.Date(as.character(activity_df$date), "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
total_steps_per_day <- tapply(activity_df$steps, activity_df$date, sum, na.rm=T)

# Make histogram of total steps per day
hist(total_steps_per_day, main="Total steps per day histogram", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Get mean and median total steps per day
total_steps_per_day_mean <- round(mean(total_steps_per_day, na.rm=T), 2)
total_steps_per_day_median <- round(median(total_steps_per_day, na.rm=T), 2)
```

Total steps per day:

- Mean: 9354.23
- Median: 10395


## What is the average daily activity pattern?

```r
activity_df <- activity_df %>%
    group_by(interval) %>%
    mutate(steps_interval_mean = mean(steps, na.rm=T)) %>%
    as.data.frame

with(activity_df, plot(interval, steps_interval_mean, type="l", main="Average daily activity", ylab="mean steps"))
```

![](PA1_template_files/figure-html/average_daily_activity-1.png)<!-- -->

## Imputing missing values

```r
# Calculate fraction of missing step data as percentage
missing_steps_logical <- is.na(activity_df$steps)

fraction_missing_data_perc <- round(mean(missing_steps_logical) * 100, 2)

# Impute missing step data with interval step means
activity_df[missing_steps_logical,]$steps <- activity_df[missing_steps_logical,]$steps_interval_mean

steps_per_day_summed <- tapply(activity_df$steps, activity_df$date, sum, na.rm=T)
hist(steps_per_day_summed, main="Total steps per day histogram (after imputation)", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
steps_per_day_summed_mean <- round(mean(steps_per_day_summed, na.rm=T), 2)
steps_per_day_summed_median <- round(median(steps_per_day_summed, na.rm=T), 2)
```

13.11 % of steps data is missing.

Total steps per day (after imputation):

- Mean: 10766.19  
- Median: 10766.19

Imputation seems to have no effect on these statistics


## Are there differences in activity patterns between weekdays and weekends?

```r
# Add factor to distinguish between weekday and weekend
weekends_logical <- weekdays(activity_df$date) %in% c("Saturday", "Sunday")
activity_df$weekday <- factor(!weekends_logical)

activity_weekday_tbl <- activity_df %>% 
    filter(weekday == TRUE) %>%
    group_by(interval) %>%
    mutate(steps_interval_mean = mean(steps))

activity_weekend_tbl <- activity_df %>% 
    filter(weekday == FALSE) %>%
    group_by(interval) %>%
    mutate(steps_interval_mean = mean(steps))

y_range <- range(range(activity_weekday_tbl$steps_interval_mean), range(activity_weekend_tbl$steps_interval_mean))

par(mfrow = c(2,1),
          oma = c(4,4,0,0) + 0.1,
          mar = c(2,1,1,1) + 0.1)
plot(activity_weekday_tbl$interval, activity_weekday_tbl$steps_interval_mean, type="l", main="Weekdays", ylim=y_range, ylab="", xlab="")
plot(activity_weekend_tbl$interval, activity_weekend_tbl$steps_interval_mean, type="l", main="Weekends", ylim=y_range, ylab="", xlab="")
title(xlab = "interval",
      ylab = "mean steps",
      outer = TRUE, line = 3)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


