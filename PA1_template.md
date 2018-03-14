Reproducible Research: Assessment 1
===================================

### Loading and preprocessing the data

1.  Read the data and process/transform the data (if necessary)

<!-- -->

    data <- read.csv('activity.csv')

1.  Inspect the dataset

<!-- -->

    summary(data)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day

<!-- -->

    step <- tapply(data$steps, data$date, sum, na.rm=TRUE)
    #step

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    hist(step, xlab = "Steps per Day", main = "Total number of steps taken each day", breaks=20, col = "blue")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    temp_mean <- mean(step)
    temp_mean <- format(temp_mean,digits=6)
    temp_median <- median(step)
    temp_median <- format(temp_median,digits=7)

-   Mean of the total number of steps taken per day: 9354.23
-   Median of the total number of steps taken per day: 10395

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

<!-- -->

    activity_pattern<- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)
    plot(activity_pattern$interval, activity_pattern$steps, type = "l",
         xlab = "Intervals", 
         ylab = "Average number of steps taken per interval", 
         main = "The average daily activity pattern")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    index<-which.max(activity_pattern$steps)
    interval<-activity_pattern$interval[index]
    max_step<-activity_pattern$steps[index]
    max_step<-round(max_step, digits = 2)

-   maximum number of steps is 206.17 corresponds to interval 835

### Imputing missing values

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    sum(is.na(data$steps))

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    nas <- data[is.na(data$steps),]
    par(mfrow = c(2,1))
    hist(nas$interval, 
         xlab = "Interval",
         main="Missing values per interval",breaks = 72)
    hist(as.numeric(nas$date), 
         xlab = "Data",
         main = "Missing values per date", breaks = 61)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

-   We notice that the missing values are distributed across all
    intervals while the missing values are located at some specific
    days. I will fill the missing values by the mean steps of correspond
    intervals.

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in by mean values.

<!-- -->

    library(plyr)
    activity_pattern<- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)
    data_missing <- data[is.na(data$steps),]
    data_full <- data[!is.na(data$steps),]
    #replace missing values with mean values
    data_missing$steps <- as.factor(data_missing$interval)
    data_missing$steps<-mapvalues(data_missing$steps,
                                  from = levels(data_missing$steps), 
                                  to = activity_pattern$steps)
    data_missing$steps<-as.integer(as.vector(data_missing$steps))
    #data_missing$steps
    data_filled <- rbind(data_missing, data_full)

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    step_original <- tapply(data$steps, data$date, sum, na.rm=TRUE)
    step_filled<-tapply(data_filled$steps, data_filled$date, sum, na.rm=TRUE)
    par(mfrow = c(1,2))
    hist(step_original, xlab = "Steps per Day", main = "Total steps taken each day (original data)", breaks=10, col = "blue",ylim=c(0,25))
    hist(step_filled, xlab = "Steps per Day", main = "Total steps taken each day (filled data)", breaks=10, col = "red",ylim=c(0,25))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

-   The frequency of low step number (0~2000) is decreased, while the
    frequency of middle step number (10000~12000) is increased!

<!-- -->

    temp_mean_filled <- mean(step_filled)
    temp_median_filled <- median(step_filled)
    temp_mean_filled <- format(temp_mean_filled,digits=7)
    temp_median_filled <- format(temp_median_filled,digits=7)
    results<- data.frame(c(temp_mean, temp_median), c(temp_mean_filled, temp_median_filled))
    colnames(results) <- c("NA removed", "NA filled")
    rownames(results) <- c("mean", "median")
    print(results)

    ##        NA removed NA filled
    ## mean      9354.23  10749.77
    ## median      10395     10641

-   Filling missing values increase the mean value about 14.92%. The
    median value is increased about 2.37%.

### Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    data_filled$date<-as.POSIXlt(data_filled$date)
    data_filled$datetype<- ifelse(data_filled$date$wday %in% c(0,6), 'weekend', 'weekday')
    data_filled$datetype<-factor(data_filled$datetype)
    #data_filled$datetype

1.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    step_filled <- aggregate(steps ~ interval + datetype, data=data_filled, mean)
    library(ggplot2)
    ggplot(step_filled, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(datetype ~ .) +
        xlab("Interval") + 
        ylab("Avarage number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)
