# Assignment 1
Jeffrey Nelson  
January 31, 2016  


```r
## First I will be loading the raw data (step1)
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
##Reformat to usable dates
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
##Remove missing values
activity_rm<-activity[which(!is.na(activity$steps)),]
##Determine numer of steps taken per day
perday<-tapply(activity_rm$steps, activity_rm$date, sum)
```


```r
hist(perday,10, main = "Total number of steps taken per day", xlab = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
## Calculate the mean and median of number of steps each day (step3)
mean(perday)
```

```
## [1] 10766.19
```

```r
median(perday)
```

```
## [1] 10765
```

```r
## Create a time series plot (step1)
dailyact<-tapply(activity_rm$steps, activity_rm$interval, mean)
plot(y = dailyact, x = names(dailyact), type = "l", xlab = "5-Minute-Interval", 
     main = "Daily Activity Pattern", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
## Find the intervale with max steps
dailyact[dailyact==max(dailyact)]
```

```
##      835 
## 206.1698
```

```r
## The answer is 835 (step2)
```

```r
## Count number of NA's
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
##Number of NA's is 230
## Replace the NAs and create a new dataset of the NA's
act_new <- activity
act_new[which(is.na(act_new$steps)),1]<-
  dailyact[as.character(act_new[which(is.na(act_new$steps)),3])]
sum(is.na(act_new))
```

```
## [1] 0
```

```r
## Compute the total number of steps each day (NA values removed)
perday_new<-tapply(act_new$steps, act_new$date, sum)
```

```r
par(mfrow=c(1,2))
hist(perday,10, main = "Total number of steps taken per day", xlab = "Steps"
     , ylim =c(0, 25))
abline(v = median(perday), col = 4, lwd = 4)
hist(perday_new,10, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", xlab = "Steps",
     ylim =c(0, 25))
abline(v = median(perday_new), col = 4, lwd = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
mean(perday_new)
```

```
## [1] 10766.19
```

```r
## Mean is 10766.19
median(perday_new)
```

```
## [1] 10766.19
```

```r
## Median is 10766.19

##We can see that the perday_new Median and Mean had very little difference from the original dataset
```

```r
##Calculate difference between weekends and weekdays
act_new$wd<-weekdays(act_new$date)
act_new$fwd<- as.factor(c("weekend", "weekday"))
act_new[act_new$wd == "Sunday" | act_new$wd == "Saturday" ,5]<- factor("weekend")
act_new[!(act_new$wd == "Sunday" | act_new$wd == "Saturday"),5 ]<- factor("weekday")
```


```r
act_new_we <- subset(act_new, fwd == "weekend") 
act_new_wd <- subset(act_new, fwd == "weekday") 
dailyact_we<-tapply(act_new_we$steps, act_new_we$interval, mean)
dailyact_wd<-tapply(act_new_wd$steps, act_new_wd$interval, mean)
par(mfrow=c(2,1))
plot(y = dailyact_wd, x = names(dailyact_wd), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(y = dailyact_we, x = names(dailyact_we), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekends", ylab = "Average number of steps", 
     ylim =c(0, 250))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

