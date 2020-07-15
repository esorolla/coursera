# Check if the library is available
if(!require("ggplot2")){
  install.packages("ggplot2")
}
library(ggplot2)

# Checking if file already exists.
if(!file.exists("activity.csv")) {
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                destfile = "data.zip")
  unzip("data.zip")
  file.remove("data.zip")
}

# Step 1. Reading data and processing the data
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# Step 2
steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

ggplot(steps_per_day, aes(steps)) + 
  geom_histogram(binwidth = 1000, color = "black") +
  labs(x = "Steps", y = "Frequency", title = "Total Steps per Day") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Step 3
mean_steps <- mean(steps_per_day$steps, na.rm = TRUE)
median_steps <- median(steps_per_day$steps, na.rm = TRUE)

# Step 4
mean_steps_per_interval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

ggplot(mean_steps_per_interval, aes(interval, steps)) + geom_line() +
  labs(x = "Interval", y = "Steps", title = "Average number of Steps per Interval") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Step 5
max_mean_interval <- mean_steps_per_interval[which.max(mean_steps_per_interval$steps), ]$interval

# Step 6. Removing NA

#To impute a missing value, we can use the mean value of the steps. 
#For each interval, we must find our own average value,
#because at different intervals the activity is different.

na_index <- is.na(activity$steps)
new_values <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE, simplify = TRUE)
activity[na_index, ]$steps <- round(new_values[as.character(activity[na_index, ]$interval)])

# Step 7
steps_per_day <- aggregate(steps ~ date, activity, sum)

ggplot(steps_per_day, aes(steps)) + 
  geom_histogram(binwidth = 1000, color = "black") +
  labs(x = "Steps", y = "Frequency", title = "Total Steps per Day (w/o NA)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Step 8
activity$weekday <- c("Weekend", "Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend")[as.POSIXlt(activity$date)$wday + 1]
mean_steps_per_interval <- aggregate(steps ~ interval + weekday, activity, mean)

ggplot(mean_steps_per_interval, aes(interval, steps, color = weekday)) + geom_line() +
  facet_wrap(.~ weekday, nrow = 2) +
  labs(x = "Interval", y = "Steps", title = "Average number of Steps per Interval (w/o NA)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), legend.position="none")
