library(ggplot2)

# Read/process data
df <- read.csv("activity.csv")
df$date <- as.Date(df$date)

#==========================
# Part 1
# Steps per day histogram
stepsPerDay <- aggregate(df$steps ~ df$date, df, sum, na.rm = TRUE)
hist(stepsPerDay$`df$steps`, breaks = 10, xlab = "Steps", main = "Steps Per Day", col="#6cabe7")

# Mean steps per day
meanSteps <- mean(stepsPerDay$`df$steps`)
meanSteps

# Median steps per day
medianSteps <- median(stepsPerDay$`df$steps`)
medianSteps

#==========================
# Part 2
avgStepsPerInt <- aggregate(df$steps ~ interval, df, mean)
plot(avgStepsPerInt$interval, avgStepsPerInt$`df$steps`, type = "l", xlab="Interval", ylab = "Steps", main = "Average Steps Per Day")

maxSteps <- avgStepsPerInt[avgStepsPerInt$`df$steps`==max(avgStepsPerInt$`df$steps`),]
maxSteps

#==========================
# Part 3
# Duplicate original data
dfFull <- df

# Calculate total missing values
missingValueCount <- sum(is.na(dfFull$steps))
missingValueCount

missingValues <- is.na(dfFull$steps)

intervalAvg <- tapply(dfFull$steps, dfFull$interval, mean, na.rm = TRUE, simplify = TRUE)
dfFull$steps[missingValues] <- intervalAvg[as.character(dfFull$interval[missingValues])]

# Make histogram
stepsPerDayFull <- aggregate(dfFull$steps ~ dfFull$date, dfFull, sum)
hist(stepsPerDayFull$`dfFull$steps`, breaks = 10, xlab = "Steps", main = "Steps Per Day", col="#6cabe7")

# Mean steps per day
meanStepsFull <- mean(stepsPerDayFull$`dfFull$steps`)
meanStepsFull

# Median steps per day
medianStepsFull <- median(stepsPerDayFull$`dfFull$steps`)
medianStepsFull

# Impact of imputing data
summary(stepsPerDay)
summary(stepsPerDayFull)

#==========================
# Part 4
dfFull$dayOfWeek <- weekdays(dfFull$date)
dfFull$dateType <- ifelse(dfFull$dayOfWeek == "Saturday" | dfFull$dayOfWeek == "Sunday", "Weekend", "Weekday")

avgStepsPerIntFull <- aggregate(dfFull$steps ~ dfFull$dateType+dfFull$interval, dfFull, mean)

plot <- ggplot(dfFull, aes(dfFull$interval, dfFull$steps)) + geom_line() + facet_wrap(~ dfFull$dateType, ncol = 1, nrow = 2) + labs(title = "Average Steps By Interval and Day Type", x = "Interval", y = "Steps")







