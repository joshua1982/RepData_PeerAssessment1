#Peer Assessment 1 for Reproducible Research#

The following report details the steps and and the answers to Peer Assignment 1 for Reproducible Research

##Loading and preprocessing the data##
```{r,echo=TRUE}
setwd("~/IDA/Reproducible Research")
data<-read.csv("activity.csv",head=TRUE,sep=",",colClasses=c("numeric","character","numeric"))
library(ggplot2)
library(dplyr)
```

##What is mean total number of steps taken per day?##

###Histogram of the total number of steps taken each day.###
```{r,echo=TRUE}
totalsteps<-data%>%group_by(date)%>%summarise_each(funs(sum)) ## this gets you the total steps taken each day.
hist(totalsteps$steps,xlab="Steps",main="Total Steps per Day") ## this creates the histogram.
```

###Mean and median total number of steps taken per day.###
```{r,echo=TRUE}
summary(totalsteps$steps)## the mean and median steps per day is reported here.
```

##What is the average daily activity pattern?##

-the following code chunk gets the mean number of steps taken over the time intervals and;
-resolves the missing values issue.

```{r,echo=TRUE}
meansteps<-data%>%group_by(date)%>%summarise_each(funs(mean)) ## mean steps taken over the time intervals
step<-data[,1] ##getting rid of the missing values
step[is.na(step)]<-0
inter<-cbind(data,step)
inter<-data.frame("interval"=inter[,3],"steps"=inter[,4])
inter<-inter%>%group_by(interval)%>%summarise_each(funs(mean))
```

###Plot for average steps across intervals.###
```{r,echo=TRUE}
par(mar=c(6,6,2,2)) ## steps at each interval (on average)
plot(inter$interval,inter$steps,xaxt="n", yaxt="n", xlab="", ylab="",type="l")
axis(1,at=pretty(inter$interval),labels=pretty(inter$interval),las=1)
axis(2,at=pretty(inter$steps),labels=format(pretty(inter$steps),big.mark=",", scientific=FALSE),las=1)
mtext(text="Interval", side=1, line=2,cex=1)
mtext(text="Steps", side=2, line=5,cex=1)
title("Steps at each Interval")
```

###5-minute interval, on average across all the days in the dataset which contains the maximum number of steps###

```{r,echo=TRUE}
order(inter$steps) ## highest number of steps
inter[104,1] ## interval with highest number of steps
```

##Imputing missing values.##

###Number of missing values in the dataset.###
```{r,echo=TRUE}
sum(is.na(data)) ## number of missing values
```

###The following code chunk resolves the missing values issue.###
```{r,echo=TRUE}
missing<-data[,1] ##getting rid of the missing values
missing[is.na(missing)]<-0
```

###New dataset with the missing values filled in.###
```{r,echo=TRUE}
full<-cbind(data,missing)
complete<-data.frame("date"=full$date,"interval"=full$interval,"steps"=full$missing)
```

###Histogram of the total number of steps taken each day and a report on the mean and median total number of steps taken per day.### 

-the readings are affected by the new data and the histogram of the steps over time interval becomes skewed, as compared to the normal distribution seen earlier.

```{r,echo=TRUE}
hist(complete$steps,xlab="Steps",main="Total Steps per Day")
summary(complete$steps) ##mean and median steps per day with the full data
```

##Are there differences in activity patterns between weekdays and weekends?##

-the following code chunks create the new datasets for the weekday and weekend readings.
```{r,echo=TRUE}
days<-weekdays(as.POSIXct(complete[,1]),abbreviate=FALSE) ##convert to weekdays and weekends
Day<-cbind(complete[,2],complete[,3],days)
Day<-data.frame("interval"=as.numeric(Day[,1]),"steps"=as.numeric(Day[,2]),"days"=Day[,3])

head(Day)
table(Day$days)

weekday<-list("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend<-list("Saturday","Sunday")              

M2F<-NULL
for(i in 1:5){
    M2F<-rbind(M2F,subset(Day,days==weekday[i]))
}
WE<-NULL
for(i in 1:2){
    WE<-rbind(WE,subset(Day,days==weekday[i]))
}
weekday<-M2F%>%group_by(interval)%>%summarise_each(funs(mean))## this is the weekday readings
weekend<-WE%>%group_by(interval)%>%summarise_each(funs(mean)) ## this is the weekend readings
```

###Panel plot showing the weekday and weekend steps over time interval readings.###
```{r,echo=TRUE}
par(mfrow=c(2,1))
par(mar=c(2,6,2,2))
plot(weekday$interval,weekday$steps,type="l",xlab="",ylab="")
mtext(text="Interval", side=1, line=2,cex=1)
mtext(text="Steps", side=2, line=5,cex=1)
title("Weekday")
par(mar=c(2,6,2,2))
plot(weekend$interval,weekend$steps,type="l",xlab="",ylab="")
mtext(text="Interval", side=1, line=2,cex=1)
mtext(text="Steps", side=2, line=5,cex=1)
title("Weekend")
```

#The End.#
