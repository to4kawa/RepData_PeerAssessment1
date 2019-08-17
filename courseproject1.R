##
## Program Name: courseproject1.R
## 
## Purpose:
## This program for Reproducible Research > Week 2 > Course Project 1
##
## 1.Code for reading in the dataset and/or processing the data
## 2.Histogram of the total number of steps taken each day
## 3.Mean and median number of steps taken each day
## 4.Time series plot of the average number of steps taken
## 5.The 5-minute interval that, on average, contains the maximum number of steps
## 6.Code to describe and show a strategy for imputing missing data
## 7.Histogram of the total number of steps taken each day after missing values are imputed
## 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## 9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


library(dplyr)
library(readr)
library(ggplot2)

## 1. Code for reading in the dataset and/or processing the data
### unzip data and read csv
### cols(steps, data, interval) are read as num,char,num

data <- {unzip("activity.zip")}
activity_df <- read_csv(data,col_types="ncn")

### transform interval into hour + minutes(2digits + 2ditits)
### create datetime as POSIXct and then create act_df

activity_df %>% mutate(interval=formatC(interval,width=4,flag="0")) %>%
  mutate(datetime=as.POSIXct(paste(date,interval),format="%Y-%m-%d %H%M")) %>%
  mutate(date=as.Date(date,"%Y-%m-%d")) -> act_df

## 2. Histogram of the total number of steps taken each day
### create hist_df

act_df %>% group_by(date) %>% 
  summarise(totalSteps= sum(steps,na.rm=TRUE)) -> hist_df

### plot 
with(hist_df, plot(date,totalSteps,type="h",lwd=8,col="red"
  ,main="Histogram of the total number of steps taken each day"))

## 3. Mean and median number of steps taken each day
### Median did not appear as it was, so 0 was excluded

act_df %>% group_by(date) %>% filter(steps!=0) %>%
  summarise(ms=mean(steps,na.rm=TRUE),mes=median(steps,na.rm=TRUE)) -> mm_df

### plot
plot(mm_df$date,mm_df$ms,type="l",xlab="Date",ylab=""
     ,main="Mean and median number of steps taken each day")
lines(mm_df$date,mm_df$mes,col="red")
legend("topleft",legend=c("Mean","Median"),col=c("black","red"),lty=1)

## 4. Time series plot of the average number of steps taken
### NaN did not appear as it was, so NaN was excluded

act_df %>% group_by(date) %>% summarise(as=mean(steps,na.rm=TRUE)) %>%
  mutate(as=if_else(is.nan(as),0,as)) -> avg_df

left_join(act_df,avg_df,by="date") -> n4_df

### plot

par(mfrow=c(2,1))
par(oma = c(0, 0, 3, 0))  
plot(n4_df$datetime,n4_df$steps
     ,col="red",type="l",main="Steps",ylab="",xlab="Date")
plot(n4_df$datetime,n4_df$as
     ,col="blue",type="h", main="Avg steps by each day",ylab="",xlab="Date")
mtext(side=3,line=1,outer=T,text="Time series plot of the average number of steps taken")






