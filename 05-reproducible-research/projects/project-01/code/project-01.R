################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 5 Reproducible Research
## Week: Week 1
## Project: Project 1
## File: project-01.R
## Date: 2016-02-11
################################################################################


################################################################################
## Part 1: Loading and preprocessing the data
################################################################################

## Show any code that is needed to:

## 1. Load the data (i.e., read.csv()).

data <- read.csv("../data/activity.csv")

## 2. Process/transform the data (if necessary) into a format 
##      suitable for your analysis.

data$date <- as.Date(as.character(data$date, "%Y-%m-%d"))


################################################################################
## Part 2: What is mean total number of steps taken per day?
################################################################################

## For this part of the assignment, you can ignore the missing values 
##  in the dataset.

## 1. Calculate the total number of steps taken per day.

library(dplyr)
num_steps_per_day <- data %>%
    group_by(date) %>%
    summarise(total_num_steps = sum(steps))

## 2. If you do not understand the difference between a histogram and a barplot, 
##      research the difference between them. 
##      Make a histogram of the total number of steps taken each day.

library(ggplot2)
qplot(num_steps_per_day$total_num_steps, 
      geom="histogram",
      fill=I("blue"),
      col=I("lightblue"),
      main="Distribution of Number of Steps per Day",
      xlab="Number of Steps",
      ylab="Frequency")

ggsave("../output/plot1.png", width=6, height=2)

## 3. Calculate and report the mean and median of the total number of steps 
##      taken per day.

mean <- mean(num_steps_per_day$total_num_steps, na.rm=T)
median <- median(num_steps_per_day$total_num_steps, na.rm=T)
cat("Mean total number of steps per day:")
cat(mean)
cat("Median total number of steps per day:")
cat(median)


################################################################################
## Part 3: What is the average daily activity pattern?
################################################################################

## 1. Make a time series plot (i.e., type="l") of the 5-minute interval (x-axis)
##      and the average number of steps taken, averaged across all days (y-axis).

library(dplyr)
num_steps_per_interval <- data %>%
    group_by(interval) %>%
    summarise(average_num_steps = mean(steps, na.rm=T))

png("../output/plot2.png", width=1800, height=600)

with(num_steps_per_interval, 
     plot(x=interval, 
          y=average_num_steps, 
          type="l",
          main="Average Number of Steps per Interval",
          xlab="Interval",
          ylab="Average Number of Steps",
          xaxt="n")
     )
axis(1, at=num_steps_per_interval$interval, tck=0.01)

dev.off()

## 2. Which 5-minute interval, on average across all the days in the dataset, 
##      contains the maximum number of steps?

max = max(num_steps_per_interval$average_num_steps)
max_average_interval <- 
    num_steps_per_interval[num_steps_per_interval$average_num_steps == max, ]$interval
cat("Interval containing max average steps:")
cat(max_average_interval)


################################################################################
## Part 4: Imputing missing values
################################################################################

## Note that there are a number of days/intervals where there are missing values
##      (coded as NA). The presence of missing days may introduce bias
##      into some calculations or summaries of the data.

## 1. Calculate and report the total number of missing values in the dataset
##      (i.e., the total number of rows with NAs).

is_na <- is.na(data)
num_na_rows <- sum(is_na)
cat("Total number of missing values: ")
cat(num_na_rows)

## 2. Devise a strategy for filling in all of the missing values in the dataset.
##      The strategy does not need to be sophisticated.
##      For example, you could use the mean/median for that day,
##      or the mean for that 5-minute interval, etc.

## average number of steps per interval

## 3. Create a new dataset that is equal to the original dataset 
##      but with the missing data filled in.

imputed_data <- data
for (i in 1:dim(imputed_data)[1]) {
    if (is.na(imputed_data[i, ])) {
        interval <- imputed_data[i, ]$interval
        idx <- which(num_steps_per_interval$interval == interval)
        imputed_data[i, ]$steps <- num_steps_per_interval[idx, ]$average_num_steps
    }
}

## 4. Make a histogram of the total number of steps taken each day 
##      and calculate and report the mean and median total number of steps 
##      taken per day. Do these values differ from the estimates 
##      from the first part of the assignment? 
##      What is the impact of imputing missing data on the estimates 
##      of the total daily number of steps?

library(dplyr)
imputed_num_steps_per_day <- imputed_data %>%
    group_by(date) %>%
    summarise(total_num_steps = sum(steps))

library(ggplot2)
qplot(imputed_num_steps_per_day$total_num_steps, 
      geom="histogram",
      fill=I("blue"),
      col=I("lightblue"),
      main="Distribution of Number of Steps per Day",
      xlab="Number of Steps",
      ylab="Frequency")

ggsave("../output/plot3.png", width=6, height=2)

mean <- mean(imputed_num_steps_per_day$total_num_steps, na.rm=T)
median <- median(imputed_num_steps_per_day$total_num_steps, na.rm=T)
cat("Mean total number of steps per day:")
cat(mean)
cat("Median total number of steps per day:")
cat(median)


################################################################################
## Part 5: Are there differences in activity patterns between weekdays 
## and weekends?
################################################################################

## For this part the weekdays() function may be of some help here.
## Use the dataset with the filled-in missing values for this part.

## 1. Create a new factor variable in the dataset with two levels – 
##      “weekday” and “weekend” indicating whether a given date is a weekday 
##      or weekend day.

library(dplyr)
imputed_data <- imputed_data %>%
    mutate(dayofweek = weekdays(date)) %>%
    mutate(daytype = 
               ifelse(dayofweek=="Saturday" | dayofweek=="Sunday", 
                      "weekend", "weekday")) %>%
    mutate(daytype = factor(daytype, levels=c("weekday","weekend")))

## 2. Make a panel plot containing a time series plot (i.e., type="l") 
##      of the 5-minute interval (x-axis) and the average number of steps taken, 
#       averaged across all weekday days or weekend days (y-axis). 
##      See the README file in the GitHub repository to see an example 
##      of what this plot should look like using simulated data.

library(dplyr)
weekday_num_steps_per_interval <- imputed_data %>%
    filter(daytype == "weekday") %>%
    group_by(interval) %>%
    summarise(daytype = "weekday",
              average_num_steps = mean(steps))

weekend_num_steps_per_interval <- imputed_data %>%
    filter(daytype == "weekend") %>%
    group_by(interval) %>%
    summarise(daytype = "weekend",
              average_num_steps = mean(steps))

merged <- rbind(weekday_num_steps_per_interval, weekend_num_steps_per_interval)

library(ggplot2)
ggplot(merged, 
       aes(x=interval, y=average_num_steps)) + 
    geom_line(col="lightblue") + 
    facet_wrap(~daytype, ncol=1) +
    theme(strip.background = element_rect(fill="orange", alpha=0.5)) +
    ggtitle("Number of Steps per Interval Weekday vs. Weekend") +
    labs(x="Interval", y="Number of Steps") 

ggsave("../output/plot4.png", width=6, height=4)

################################################################################