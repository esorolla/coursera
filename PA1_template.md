# Peer-graded Assignment: Course Project 1

## Prepairing part

Installation of all necessary libraries


```r
if(!require("ggplot2")){
  install.packages("ggplot2")
}
library(ggplot2)
```

Download required files


```r
if(!file.exists("activity.csv")) {
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                destfile = "data.zip")
  unzip("data.zip")
  file.remove("data.zip")
}
```

## Main Part

### Step 1

Reading and Processing data


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

### Step 2

Histogram of the total number of steps taken each day


```r
steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

ggplot(steps_per_day, aes(steps)) + 
  geom_histogram(binwidth = 1000, color = "black") +
  labs(x = "Steps", y = "Frequency", title = "Total Steps per Day") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
```

![](https://github.com/SUMin-Oleksandr/Reproducible-Research-Project-1/blob/master/figure/step%202-1.png)

### Step 3

Mean and median number of steps taken each day


```r
mean_steps <- mean(steps_per_day$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median_steps <- median(steps_per_day$steps, na.rm = TRUE)
```

```
## [1] 10765
```

### Step 4

Time series plot of the average number of steps taken


```r
mean_steps_per_interval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

ggplot(mean_steps_per_interval, aes(interval, steps)) + geom_line() +
  labs(x = "Interval", y = "Steps", title = "Average number of Steps per Interval") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
```

![](https://github.com/SUMin-Oleksandr/Reproducible-Research-Project-1/blob/master/figure/step%204-1.png)

### Step 5

The 5-minute interval that, on average, contains the maximum number of steps


```r
max_mean_interval <- mean_steps_per_interval[which.max(mean_steps_per_interval$steps), ]$interval
```

```
## [1] 835
```

### Step 6

Code to describe and show a strategy for imputing missing data

To impute a missing value, we can use the mean value of the steps. 
For each interval, we must find our own average value, because at different intervals the activity is different.


```r
na_index <- is.na(activity$steps)
new_values <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE, simplify = TRUE)
activity[na_index, ]$steps <- round(new_values[as.character(activity[na_index, ]$interval)])
```

### Step 7

Histogram of the total number of steps taken each day after missing values are imputed


```r
steps_per_day <- aggregate(steps ~ date, activity, sum)

ggplot(steps_per_day, aes(steps)) + 
  geom_histogram(binwidth = 1000, color = "black") +
  labs(x = "Steps", y = "Frequency", title = "Total Steps per Day (w/o NA)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
```

![](https://github.com/SUMin-Oleksandr/Reproducible-Research-Project-1/blob/master/figure/step%207-1.png)

### Step 8

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
activity$weekday <- c("Weekend", "Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend")[as.POSIXlt(activity$date)$wday + 1]
mean_steps_per_interval <- aggregate(steps ~ interval + weekday, activity, mean)

ggplot(mean_steps_per_interval, aes(interval, steps, color = weekday)) + geom_line() +
  facet_wrap(.~ weekday, nrow = 2) +
  labs(x = "Interval", y = "Steps", title = "Average number of Steps per Interval (w/o NA)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), legend.position="none")
```

![](https://github.com/SUMin-Oleksandr/Reproducible-Research-Project-1/blob/master/figure/step%208-1.png)

## Conclusions

All code can be viewed in the Script.R file.

