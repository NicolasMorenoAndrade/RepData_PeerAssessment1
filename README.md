---
title: "Activity Patterns"
subtitle: "Project 1 in Reproducible Research Course"
author: "Nicolas Moreno"
date: "May 28, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(scipen = 1, digits = 3)
```

## Loading and preprocessing the data

The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

```{r load, cache=FALSE, warning=FALSE, message=FALSE}
setwd("~/Desktop/DataScience/RepData_PeerAssessment1/")
if(!file.exists("activity.csv")){
system("unzip activity.zip")
}
activity <- read.csv("activity.csv")
library(dplyr)
library(ggplot2)
library(lattice)
library(lubridate)
```

## Mean total number of steps per day

```{r meantotalsteps}

totalsteps <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
totalstepsmean <- round(mean(totalsteps$steps),2)
totalstepsmedian <- median(totalsteps$steps)

```

```{r totalstepshistogram}
ggplot(data=totalsteps, aes(steps)) +
  geom_histogram(breaks=seq(0,25000,by=5000)) 
```

The mean total number of steps is `r round(totalstepsmean,2)` and the median is `r totalstepsmedian`.

## Average daily activity pattern 


```{r averageDailyActivity}
totalstepsinterval <- aggregate(steps ~ interval, data=activity, FUN = mean)
maxinterval <- totalstepsinterval[which.max(totalstepsinterval$steps),]
plot(totalstepsinterval$interval,totalstepsinterval$steps, type="l", xlab="Interval", ylab="Steps",main="Mean Steps per Day by Interval")

```

On average across all the days in the dataset, the maximum number of steps is `r maxinterval[,"steps"]` and correspond to interval `r maxinterval[,"interval"]`.

## Imputing missing values

```{r}
naNumber <- sum(!complete.cases(activity))
```

There are `r naNumber` missing values in the data set. The missing values are going to be filled with the mean of the corresponding 5 minute interval.

```{r fillMissingValues}
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity$stepsisna <- is.na(activity$steps)
activity %>%
    group_by(interval) %>%
    mutate(steps = impute.mean(steps)) -> activityNoNAs
```

```{r meantotalstepsNoNas}

totalstepsNoNas <- aggregate(steps ~ date, data = activityNoNAs, FUN = sum, na.rm = TRUE)
totalstepsmeanNoNas <- round(mean(totalstepsNoNas$steps),2)
totalstepsmedianNoNas <- round(median(totalstepsNoNas$steps),3)
totalstepsNoNas$imputed <- "Imputed data"
totalsteps$imputed <- "Original Data"
bindedsteps <- rbind(totalstepsNoNas, totalsteps)

```

```{r totalstepshistogramNoNas}
ggplot(data=bindedsteps, aes(steps, fill=imputed)) +
  geom_histogram(breaks=seq(0,25000,by=5000)) +
  facet_grid(. ~ imputed) +
  theme(legend.position = 'none')
```

The mean total number of steps for the activity data after replacement of NAs is `r round(totalstepsmeanNoNas,2)` and the median is `r round(totalstepsmedianNoNas,3)`. The difference is very small when compared with the values from the original data. The mean doesn't change, and the median has a small change. The new median is now almost identical to the mean. This could be due to the fact that I use means for intervals, thus get more data points quite similar or identical to the mean, and so the median is so shifted to become almost equal to the mean. Probably it would be different if I had used the daily mean or some other strategy to fill the NA values. 

## Are there differences in activity patterns between weekdays and weekends?


```{r diffWeekdays}
activityNoNAs$dayName <- wday(activityNoNAs$date, label = TRUE)
activityNoNAs$dayType <- ifelse(activityNoNAs$dayName%in%c("Sat","Sun"), "Weekend", "Weekday")

meanstepsWeekday <- aggregate(steps ~ interval + dayType, activityNoNAs, mean)

xyplot(meanstepsWeekday$steps ~ meanstepsWeekday$interval|meanstepsWeekday$dayType, main="Mean Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

The plot shows that there's a peak of activity (steps) early during weekdays. However overall there is more activity during weekends. 


## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository and the SHA-1 commit ID for your
repository state.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.



### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data (if necessary) into a format suitable for your analysis


### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the **mean** and **median** total number of steps taken per day


### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![Sample panel plot](instructions_fig/sample_panelplot.png) 


**Your plot will look different from the one above** because you will
be using the activity monitor data. Note that the above plot was made
using the lattice system but you can make the same version of the plot
using any plotting system you choose.


## Submitting the Assignment

To submit the assignment:

1. Commit your completed `PA1_template.Rmd` file to the `master` branch of your git repository (you should already be on the `master` branch unless you created new ones)

2. Commit your `PA1_template.md` and `PA1_template.html` files produced by processing your R markdown file with the `knit2html()` function in R (from the **knitr** package)

3. If your document has figures included (it should) then they should have been placed in the `figure/` directory by default (unless you overrode the default). Add and commit the `figure/` directory to your git repository.

4. Push your `master` branch to GitHub.

5. Submit the URL to your GitHub repository for this assignment on the course web site.

In addition to submitting the URL for your GitHub repository, you will
need to submit the 40 character SHA-1 hash (as string of numbers from
0-9 and letters from a-f) that identifies the repository commit that
contains the version of the files you want to submit. You can do this
in GitHub by doing the following:

1. Go into your GitHub repository web page for this assignment

2. Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

3. You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.

A valid submission will look something like (this is just an **example**!)

```r
https://github.com/rdpeng/RepData_PeerAssessment1

7c376cc5447f11537f8740af8e07d6facc3d9645
```

