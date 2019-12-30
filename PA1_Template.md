---
output: 
  html_document: 
    keep_md: yes
---


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data<-read.csv("activity.csv")
data$date<-as.Date(data$date)
```

## What is mean total number of steps taken per day?  

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day:


```r
TotalSteps<- aggregate(data$steps,by=list(data$date), FUN=sum)
colnames(TotalSteps)<-c("Date","Steps")

hist(TotalSteps$Steps, 
     main="Daily Total Steps",
     xlab="Total Steps in a Day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(TotalSteps$Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 835


```r
avgInt<-aggregate(data$steps, by=list(data$interval),FUN=mean,na.rm=TRUE)
colnames(avgInt)<-c("Interval","AvgSteps")
plot(avgInt$Interval,avgInt$AvgSteps,type="l")
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
MaxSteps<- avgInt[which.max(avgInt$AvgSteps),]
MaxSteps
```

```
##     Interval AvgSteps
## 104      835 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

Now there are no NAs so the NAs are now being counted in the histogram thus increasing frequency. The median has increased by 1 but mean is unaffected. 


```r
TotalNA<-sum(is.na(data))
TotalNA
```

```
## [1] 2304
```

```r
Joined<-left_join(data,avgInt,by=(c("interval"="Interval")))
Joined$FilledSteps<-coalesce(as.numeric(Joined$steps),Joined$AvgSteps)

JoinedTotal<-aggregate(Joined$FilledSteps,by=list(Joined$date), FUN=sum)
colnames(JoinedTotal)<-c("Date","TotalSteps")

hist(JoinedTotal$TotalSteps)
```

![](PA1_Template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
summary(JoinedTotal$TotalSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
Joined$Week<-weekdays(Joined$date)
Joined$Weekday<-as.factor(ifelse(Joined$Week=="Saturday","weekend",ifelse(Joined$Week=="Sunday","weekend","weekday")))

weekend<-subset(Joined,Joined$Weekday=="weekend")
weekday<-subset(Joined,Joined$Weekday=="weekday")

weekendAvg<-aggregate(weekend$FilledSteps, by=list(weekend$interval),FUN=mean)
weekdayAvg<-aggregate(weekday$FilledSteps, by=list(weekday$interval),FUN=mean)

colnames(weekendAvg)<-c("WeekendInterval","AvgSteps")
colnames(weekdayAvg)<-c("WeekdayInterval","AvgSteps")

par(mfrow=c(1,2))
plot(weekendAvg$WeekendInterval,weekendAvg$AvgSteps,type="l", main="Weekend Avg", ylim = c(0,300))
plot(weekdayAvg$WeekdayInterval,weekdayAvg$AvgSteps,type="l", main= "Weekday Avg", ylim = c(0,300))
```

![](PA1_Template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


