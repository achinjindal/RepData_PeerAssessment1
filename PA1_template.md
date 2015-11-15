# Reproducible Research: Peer Assessment 1

This is Achin Jindal's submission for PA1 for the course Reproducible Research.

## Loading and preprocessing the data


```r
par(mfrow=c(2,1))
data=read.csv("activity.csv")
  
temp=as.Date(data$date, format="%F")
data$date=temp
  
data_noNA=data[!is.na(data$steps),]
```

## What is mean total number of steps taken per day?


```r
stepsPerDay=tapply(data_noNA$steps, data_noNA$date, sum)
  
hist(stepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
print(median(stepsPerDay))
```

```
## [1] 10765
```

```r
print(mean(stepsPerDay))
```

```
## [1] 10766.19
```

## What is the average daily activity pattern?


```r
stepsPerInterval=tapply(data_noNA$steps, data_noNA$interval, mean)
plot(names(stepsPerInterval), stepsPerInterval, type = "l", xlab="Interval", ylab="Number of Steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxInterval=names(which(stepsPerInterval== max(stepsPerInterval)))
```

## Imputing missing values


```r
NA_count=nrow(data[is.na(data$steps),])
print(NA_count)
```

```
## [1] 2304
```

```r
data_NAreplaced=data

stepsPerIntervalMedian=tapply(data_noNA$steps, data_noNA$interval, median)
for(i in which(is.na(data$steps),)) data_NAreplaced[i,1]=stepsPerIntervalMedian[which(names(stepsPerIntervalMedian)==data[i,3])]

#nrow(data_NAreplaced[is.na(data2[,1]),])

stepsPerDay2=tapply(data_NAreplaced$steps, data_NAreplaced$date, sum)
hist(stepsPerDay2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
print(median(stepsPerDay2))
```

```
## [1] 10395
```

```r
print(mean(stepsPerDay2))
```

```
## [1] 9503.869
```

## Are there differences in activity patterns between weekdays and weekends?
