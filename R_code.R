## 1. Loading and preprocessing the data
        ifelse(!dir.exists("~/data/RepData_PeerAssessment1/"), dir.create("~/data/RepData_PeerAssessment1/"), FALSE)
        setwd("./data/RepData_PeerAssessment1/")
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(url,destfile="./data1.zip", method = "curl")
        
        ##unzip and read data
        data1 <- unzip ("data1.zip", exdir = "./")
        dir()
        data <- read.csv(data1, header = T, stringsAsFactors = FALSE)
        head(data)
        str(data)

## 2. What is mean total number of steps taken per day?
library(dplyr)

        ##summarize by date and take mean step count
        step <- data %>%  group_by(date) %>% summarize(sum(steps, na.rm = TRUE))
        
        ##Plot the total steps in histogram
        par(mfrow = c(1,1))
        hist(step$sum, main = "Total steps per day")
        
        ## Summary statistics of steps 
        summary(step$sum)

## 3. What is the average daily activity pattern?
        steppat <- step <- data %>%  group_by(interval) %>% summarize(mean(steps, na.rm = TRUE))
        
        ##Plot the step pattern
        par(mfrow = c(1,1))
        plot(steppat$interval, steppat$mean, type = "l", xlab = "Interval", ylab = "Average Step Count", main = "Total steps per day")
 
## 4. Imputing missing values
        ## count of rows with missing values
        miss <- data[is.na(data),]
        nrow(miss)
        
        ##Create new data frame with imputed values
        data_imp <- data ## store data in a new data frame
        a <- rep(steppat$mean, 61) ## create a vector with mean step counts for 61 days
        a[is.na(data$steps)] -> data_imp$steps[is.na(data$steps)] ## replace NA with mean step counts 
        
        ##sum of steps per day and histogram of the new data set
        step_imp <- data_imp %>%  group_by(date) %>% summarize(sum(steps, na.rm = TRUE))
        hist(step_imp$sum, main = "Total steps per day")  ##Plot the total steps in histogram
        
        ##summary stats of the new data set
        summary(step_imp$sum)

## 5. Are there differences in activity patterns between weekdays and weekends?
        ## determine the weekdays
        library(lubridate)
        data_imp$date <- ymd(data_imp$date)
        data_wkd <- data_imp %>%  mutate(weekday = weekdays(date)) %>% 
                mutate(weektime = ifelse(weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday"))
        df <- ddply(data_wkd, c("weektime", "interval"), summarise, mean = mean(steps))
        ##plot the steps
        library(ggplot2)
        qplot(interval, mean, data = df, col = weektime, geom = c ("line", "smooth"), 
              main = "Average number of steps on weekdays vs. weekends", ylab = "Average number of steps")
        
        a <- df[data_wkd$weektime == "weekday",]
        b <- df[!data_wkd$weektime == "weekday",]
        plot(a$interval, a$mean, col = "navy", type = "l")
        lines(b$interval, b$mean, col = "red", lwd= 5)
        summary(a$weektime)
        p + geom_line()
        plot(df$interval, df$mean, col = c("navy", "red"), xlab = "Interval", ylab = "Average Steps", type = "l")
        
        