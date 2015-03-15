---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

<!-- rmarkdown v1 -->

## Loading and preprocessing the data

```r
library(dplyr)
data <- tbl_df(read.csv("activity.csv"))
```

## 1. What are the mean & median total number of steps taken per day?

```r
data <- mutate(data,date=as.Date(date,"%Y-%m-%d"))
out <- data %>% group_by(date) %>% summarise(count = n(), total = sum(steps,na.rm=TRUE))
```

#### Calculate mean and median 

```r
mean(out$total[!out$total==0]) #sum() sums NAs to 0
```

```
## [1] 10766.19
```

```r
median(out$total[!out$total==0])
```

```
## [1] 10765
```

#### Histogram of total steps

```r
hist(out$total[!out$total==0],ylim=c(0,30),xlab="Total Steps",main="Total Steps Taken Each Day")
```

<img src="figure/Histogram of Total Steps Each Day-1.png" title="plot of chunk Histogram of Total Steps Each Day" alt="plot of chunk Histogram of Total Steps Each Day" style="display: block; margin: auto;" />

## 2. What is the average daily activity pattern?

```r
library(lubridate)

## Change interval into time format
int <- sprintf("%04d", data$interval) #add leading zeros
day <- as.POSIXct(strptime(paste(data$date,int), "%Y-%m-%d %H%M")) 

## Calculate mean (manually remove # of NAs since not using mean(na.rm=TRUE))
out2 <- data %>% group_by(interval) %>% summarise(count=n(), NAs=sum(is.na(steps)),
                  total.steps=sum(steps,na.rm=TRUE), avg=total.steps/(count-NAs))

## Plot 
plot(day[1:288],out2$avg,type="l",xaxt="n", main = "Hourly Activity Pattern",
     xlab="Time of Day (hour)", ylab="Average Number of Steps Taken")
r <- as.POSIXct(round(range(day[1:288]), "hours"))
axis.POSIXct(1, at=seq(r[1], r[2], by="4 hours"), format="%H:%M")
```

<img src="figure/Average Daily Activity Pattern-1.png" title="plot of chunk Average Daily Activity Pattern" alt="plot of chunk Average Daily Activity Pattern" style="display: block; margin: auto;" />

#### Which time interval has max number of steps?

```r
maxi <- as.character(round(max(out2$avg),digits=2))
time <- substr(strftime(day[which(out2$avg==max(out2$avg))],"%H:%M:%S"),1,5)
```
#### <center> The time 08:35AM has the maximum number of steps daily (206.17) on average. </center>

## 3. Imputing missing values

#### 1. How many NA values are in the data?

```r
sum(is.na(data$steps)) #Number of NAs in data
```

```
## [1] 2304
```

#### 2. Replace missing values with (rounded) mean number of steps for each interval

```r
data2 <- data %>% group_by(interval) %>% 
  mutate(new.steps = ifelse(is.na(steps), 
                            round(sum(steps,na.rm=TRUE)/(nrow(out)-sum(is.na(steps)))), steps))
data2 <- select(data2,date:new.steps) #select all but old steps variable
```

#### 3. Calculate new mean & median for data & create histogram of total steps taken daily

```r
out3 <- data2 %>% group_by(date) %>% summarise(count = n(), total.steps = sum(new.steps))

mean(out3$total.steps)
```

```
## [1] 10765.64
```

```r
median(out3$total.steps)
```

```
## [1] 10762
```

```r
hist(out3$total, ylim=c(0,40),
     xlab="Total Steps", main="Total Steps Taken Each Day (NAs Imputed)")
```

<img src="figure/Total Steps Taken Each Day (NAs Imputed)-1.png" title="plot of chunk Total Steps Taken Each Day (NAs Imputed)" alt="plot of chunk Total Steps Taken Each Day (NAs Imputed)" style="display: block; margin: auto;" />

#### <center> Imputing NAs with the rounded mean of steps for each interval shifts the distribution to the left slightly, as the values of the mean & median both decreased. </center>

## 4. Are there differences in activity patterns between weekdays and weekends?

#### Compute average number of steps taken, averaged across all weekday days or weekend days


```r
# Recall from Question 2
# int <- sprintf("%04d", data$interval) #add leading zeros
# day <- as.POSIXct(strptime(paste(data$date,int2), "%Y-%m-%d %H%M")) 

#Build data frame
dat <- as.data.frame(select(data2,new.steps))
dat$newdate <- strftime(day, "%H:%M:%S") #interval in time format; convert for group_by()
day.type <- as.factor(ifelse(weekdays(day) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
dat$day.type <- day.type
dat <- tbl_df(dat)

out4 <- dat %>% group_by(newdate,day.type) %>% summarise(avg = mean(new.steps))
```

#### Plot panels

```r
library(ggplot2)
library(scales)
g <- ggplot(out4, aes(strptime(newdate,"%H:%M"),avg))
g + geom_line(aes(group=day.type,color=day.type)) + facet_grid(day.type ~ .) +
  scale_x_datetime(breaks=date_breaks('2 hour'),labels=date_format('%H:%M')) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(title="Hourly Activity Pattern (NAs Imputed)", 
       x="Hour of Day",y="Average Number of Steps") 
```

<img src="figure/Hourly Activity Pattern by Weekday or Weekend (NAs Imputed)-1.png" title="plot of chunk Hourly Activity Pattern by Weekday or Weekend (NAs Imputed)" alt="plot of chunk Hourly Activity Pattern by Weekday or Weekend (NAs Imputed)" style="display: block; margin: auto;" />

#### <center> Activity during the weekend is less than during weekdays which suggests that the subject works from Monday-Friday and rests on Saturday & Sunday. </center>
