Reproducible Research - Week 2 Project
--------------------------------------

Loading and preprocessing the data
----------------------------------

Setting the working directory to the appropriate folder on my machine
and load the relevant libraries.

    setwd('/Users/arushiagarwal/Documents/Coursera/John Hopkins course/Reproducible Research')
    library("ggplot2")
    library("dplyr")

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library("grid")
    library("gridExtra")

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

1.  Load and pre-process the data

<!-- -->

    unzip('week2.zip')
    data <- read.csv('activity.csv', header = TRUE)

1.  Looking at the summary of data and top 5 rows to get idea of the
    dataset

<!-- -->

    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    summary(data)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    data$dates <- as.Date(data$date ,"%Y-%m-%d")

    ## Warning in strptime(x, format, tz = "GMT"): unknown timezone 'default/Asia/
    ## Kolkata'

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Calculate the total number of steps taken per day

<!-- -->

    steps <- aggregate(data$steps, by = list(data$dates), FUN = sum)
    names(steps) <- c("Date","Total")

1.  If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day

<!-- -->

    hist <- ggplot(steps, aes(x = Total)) +
      geom_histogram(fill = "blue", binwidth = 1000) +
      labs(title = "Histogram of Daily Total Steps", x = "Steps", y = "Frequency")
    print(hist)

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    steps$mean_steps <- mean(steps$Total, na.rm = TRUE)
    steps$median_steps <- median(steps$Total, na.rm = TRUE)

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis) type = “l” means the plot is line graph

<!-- -->

    interval <- aggregate(steps ~ interval, data = data, FUN =mean)
    ggplot(interval, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Average Daily Steps", x = "Time Interval ((5 Minutes is an unit))", y = "Avg. Steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    maxinterval <- interval[interval$steps == max(interval$steps, na.rm = TRUE), 1]

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    missing_count <- sum(is.na(data))

1.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    replace_with_mean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
    meanday <- (data %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps)))
    head(meanday)

    ## # A tibble: 6 x 4
    ## # Groups:   interval [6]
    ##       steps       date interval      dates
    ##       <dbl>     <fctr>    <int>     <date>
    ## 1 1.7169811 2012-10-01        0 2012-10-01
    ## 2 0.3396226 2012-10-01        5 2012-10-01
    ## 3 0.1320755 2012-10-01       10 2012-10-01
    ## 4 0.1509434 2012-10-01       15 2012-10-01
    ## 5 0.0754717 2012-10-01       20 2012-10-01
    ## 6 2.0943396 2012-10-01       25 2012-10-01

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    imputeddata <- meanday[,c(1,4,3)]
    summary(imputeddata)

    ##      steps            dates               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0

4.Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

    newsteps <- aggregate(imputeddata$steps, by = list(imputeddata$dates), FUN = sum)
    names(newsteps) <- c("Date","Total")
    newhist <- ggplot(newsteps, aes(x = Total)) +
      geom_histogram(fill = "blue", binwidth = 1000) +
      labs(title = "Histogram of Daily Total Steps", x = "Steps", y = "Frequency")
    print(newhist)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    newsteps$mean_steps <- mean(newsteps$Total, na.rm = TRUE)
    newsteps$median_steps <- median(newsteps$Total, na.rm = TRUE)

    grid.arrange(hist, newhist, ncol = 2)

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-2.png)

Comparing mean and median

    mean(na.omit(steps$Total))

    ## [1] 10766.19

    median(na.omit(steps$Total))

    ## [1] 10765

    mean(newsteps$Total)

    ## [1] 10766.19

    median(newsteps$Total)

    ## [1] 10766.19

From the comparison, we can see that the highest count of the new
version data is larger than the one we have with NAs. The means of each
dataset are same. The medians of each dataset are slightly different.

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    imputeddata$WeekendOrWeekday <- ifelse(weekdays(imputeddata$dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

1.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated

<!-- -->

    imputeddata <- (imputeddata %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
    ggplot(imputeddata, mapping = aes(x = interval, y = Mean)) + geom_line() +
      facet_grid(WeekendOrWeekday ~.) + xlab("Interval") + ylab("Mean of Steps") +
      ggtitle("Comparison of Average Number of Steps in Each Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)

There are differences in activity patterns between weekdays and
weekends. Compared with weekdays, during weekends, people tend to get up
late and stay up late. On weekends, people tend to be more active, since
their steps are not stable caused of not working.
