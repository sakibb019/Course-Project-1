1- Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis


data <- read.csv("activity.csv")
str(data)

head(data)
attach(data)
totalPerDay <- tapply(steps,list(date),sum)
detach(data)

## 2- What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day.
Make a histogram of the total number of steps taken each day.
Calculate and report the mean and median of the total number of steps taken per day.
For this part of the assignment the missing values can be ignored.

hist(totalPerDay,breaks=25)

meanPerDay = mean(totalPerDay,na.rm = TRUE)
meanPerDay

medianPerDay <- median(totalPerDay,na.rm = TRUE)
medianPerDay

## 3- What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

attach(data)
meanEvery5min <- tapply(steps,list(interval),mean,na.rm=TRUE)
detach(data)

plot(meanEvery5min,type="l")

which.max(meanEvery5min)

meanEvery5min[104]

max(meanEvery5min)


## 4-Imputing missing values

found the total number of missing values in the dataset

calMissing <- sum(is.na(data$steps))
calMissing
##---------------------------------------
na <- is.na(data$steps)
shiftmeanEvery5min <- c(meanEvery5min[288],meanEvery5min[1:287])
imputedSteps <- rep(0,length(data$steps))
for (i in 1:length(data$steps))
{
        if(na[i])
        {
                imputedSteps[i] <- shiftmeanEvery5min[i%%length(meanEvery5min)+1]
        }
        else
        {
                imputedSteps[i] <- data$steps[i]
        } 
}

#data <- transform(data, data$imputedSteps = imputedSteps)
data <- cbind(data,imputedSteps)
str(data)


##----------------
head(data)

## Make a histogram of the total number of steps taken each day

attach(data)

totalPerDay2 <- tapply(imputedSteps,list(date),sum)
detach(data)
hist(totalPerDay2,breaks= 25)

mean(totalPerDay2)

median(totalPerDay2)

We found that the mean and median value in the  data  not differ very much from in the original data. same as the histogram does not differ too much with the histogram of the original data .

## Are there differences in activity patterns between weekdays and weekends?

date <- levels(data$date)
daytype <- weekdays(as.Date(date))
for(i in 1:length(daytype))
{
        if(daytype[i] %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        {
                daytype[i] <- "weekday"
        }
        else
        {
                daytype[i] <- "weekend"
        }
}
daytype <- as.factor(daytype)
isweekday <- rep(daytype,each=length(meanEvery5min))

data <- cbind(data,isweekday)
str(data)

##----------------
head(data)

attach(data)

daytypeDiff <- aggregate(imputedSteps, list(interval,isweekday), mean)
detach(data)
library(lattice)

xyplot(x ~ Group.1|Group.2,data=daytypeDiff,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps")


