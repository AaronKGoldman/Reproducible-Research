title: “PA1_template” 
author: Aaron Goldman output: 
html_document date: “2024-11-16” —

R Markdown
##Assignment Instructions 1.Code for reading in the dataset and/or processing the data 2.Histogram of the total number of steps taken each day 3.Mean and median number of steps taken each day 4.Time series plot of the average number of steps taken 5.The 5-minute interval that, on average, contains the maximum number of steps 6.Code to describe and show a strategy for imputing missing data 7.Histogram of the total number of steps taken each day after missing values are imputed 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends 9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

##Step 1 ##Code for reading in the dataset and/or processing the data

# Set the working directory to the folder containing your file
setwd("C:/Users/milli/OneDrive/Desktop/")

# Read the CSV file
data <- read.csv("activity.csv")

# Display the first few rows of the data
head(data)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
Exploring the basics of this data # Set the working directory (if necessary) setwd(“C:/Users/milli/OneDrive/Desktop/”)

# Load the data
activity <- read.csv("activity.csv")

# Exploring the basics of this data
dim(activity)
## [1] 17568     3
names(activity)
## [1] "steps"    "date"     "interval"
head(activity)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
str(activity)
## 'data.frame':    17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
# Total number of missing data
sum(is.na(activity$steps)) / dim(activity)[[1]]
## [1] 0.1311475
# Transforming the date column into date format using lubridate
library(lubridate)
## 
## Attaching package: 'lubridate'
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
activity$date <- ymd(activity$date)
length(unique(activity$date))
## [1] 61
1. Calculate the total number of steps taken per day
To understand the overall activity level, we first calculate the total number of steps taken each day.

total_steps_per_day <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
2. Make a Histogram of the Total Number of Steps Taken Each Day
A histogram visualizes the distribution of the total steps taken per day, helping us see the frequency of different activity levels.

Load necessary library
library(ggplot2)

# Make a histogram
ggplot(total_steps_per_day, aes(x = steps)) + 
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Total Number of Steps Taken Each Day", x = "Total Steps", y = "Frequency") +
  theme_minimal()
 ![download](https://github.com/user-attachments/assets/a0134c14-d789-485b-8b63-d724105c2930)

 


