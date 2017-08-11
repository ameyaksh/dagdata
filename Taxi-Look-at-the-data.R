#setwd("F:/kaggle/nyc") # set the working directory

setwd("C:/Users/akshirsa/Desktop/July - 2017/nyc")

#library(ggplot2)
library(dplyr)

train <-  read.csv("train.csv") # load the training data
test <- read.csv("test.csv") # load the test data

#str(train) # look at the structure of the training data
#str(test) # look at the structure of the test data

# date_time variable is considered as a factor
# let's split it in two variables : time & date

train$Pickup_date <- as.Date(train$pickup_datetime)
test$Pickup_date <-   as.Date(test$pickup_datetime)
train$Dropoff_date <- as.Date(train$dropoff_datetime)
train$Pickup_time <-  as.POSIXct(train$pickup_datetime)
test$Pickup_time <-  as.POSIXct(test$pickup_datetime)
train$Dropoff_time <- as.POSIXct(train$dropoff_datetime)

#let us check the datatype of the recently created variables

#str(train) # look at the structure of the training data
#str(test) # look at the structure of the test data

# Perfect ! Because R works in memory it will be helpful if remove unnecessary variables

train$pickup_datetime <- NULL
train$dropoff_datetime <-  NULL
test$pickup_datetime <- NULL

# Vendor id shows up as int . But it takes only 2 values . hence convert it to factor 

train$vendor_id <- as.factor(train$vendor_id)
test$vendor_id <- as.factor(test$vendor_id)

# We can calculate the distance between pickup and dropoff using the latitude and longitudes
# We use haversine distance to compute it
# we'll need geosphere package for that 

library(geosphere)

train$haver <-  distHaversine(train[,4:5],train[,6:7])
test$haver <-  distHaversine(test[,4:5],test[,6:7])

# We no longer need latitude , longitude columns 

train$pickup_longitude <-  NULL
train$pickup_latitude <-  NULL
train$dropoff_longitude <-  NULL
train$dropoff_latitude <-  NULL
test$pickup_longitude <- NULL
test$pickup_latitude <-  NULL
test$dropoff_longitude <-  NULL
test$dropoff_latitude <-  NULL

# Let us clear out all the outliers 

#boxplot(train$trip_duration)
#plot(train$trip_duration)

#lr1 <- lm(trip_duration ~ haver, data=train)
#summary(lr1)
#plot(resid(lr1))
#summary(resid(lr1))
#boxplot(resid(lr1))

# clearly we have outliers in trip duration 
q <- quantile(train$trip_duration)

#q[4] gives the value for 3rd quartile 
#outlier = 

train <- train[!(train$trip_duration > (q[4] + IQR(train$trip_duration)*1.5)),]
train <- train[!(train$trip_duration < (q[2] - IQR(train$trip_duration)*1.5)),]

#lr2 <- lm(trip_duration ~ haver, data=train)
#summary(lr2)
#boxplot(resid(lr2))

w <- quantile(train$haver)

train <- train[!(train$haver >( w[4] + IQR(train$haver)*1.5 )),]

# i am assuming that people will hail a atxi atleast for 100 m , hence removing everything which is less than that
train <- train[!(train$haver<=100),]
#plot(train$haver)
#lr3 <- lm(trip_duration ~ haver, data=train)
#summary(lr3)

#lr4 <- lm(trip_duration ~ haver + passenger_count, data=train)
#summary(lr4)

# lr 3= 0.4637 & lr4 = .46 so hardly any improvement
# lets us remove 0 passenger records & 7 & above .. 

train <- train[!(train$passenger_count==0),]
train <- train[!(train$passenger_count>=7),]


#plot(lr4)
#lr5 <- lm(trip_duration ~ haver + passenger_count, data=train)
#summary(lr5)

#there is no difference in the adjusted r square values of lr4 & lr5 

#let us try using date as varibale. First we need to convert it to numeric
#and our origin is the first date of the train data 2016-01-01

org_date <- as.Date('2016-01-01',"%Y-%m-%d")
org_time <- as.POSIXct(strptime('2016-04-01 00:00:00',"%Y-%m-%d %H:%M:%S"))

train$Pickup_date2 <- as.numeric(train$Pickup_date - org_date +1 )
train$Pickup_time2 <- as.numeric(train$Pickup_time) - as.numeric(org_time)

test$Pickup_date2 <- as.numeric(test$Pickup_date - org_date +1 )
test$Pickup_time2 <- as.numeric(test$Pickup_time) - as.numeric(org_time)

#boxplot(train$Pickup_time2) is perfect
#boxplot(train$Pickup_date2)  is perfect

#lr6 <- lm(trip_duration ~ haver + passenger_count + Pickup_date2 + Pickup_time2 , data=train)
#summary(lr6)

#plot(resid(lr6))


train$wd <- as.integer(as.POSIXlt(train$Pickup_date)$wday)
test$wd <-as.integer( as.POSIXlt(test$Pickup_date)$wday)


#lr6 <- lm(trip_duration ~ haver + passenger_count + wd + Pickup_time2 , data=train)
#summary(lr6)


train$month <- as.integer(format(as.Date(train$Pickup_date),"%m"))
test$month <- as.integer(format(as.Date(test$Pickup_date),"%m"))



time_q1 <- as.POSIXct(strptime('2016-04-01 00:00:00',"%Y-%m-%d %H:%M:%S"))
time_q2 <- as.POSIXct(strptime('2016-04-01 03:00:00',"%Y-%m-%d %H:%M:%S"))
time_q3 <- as.POSIXct(strptime('2016-04-01 06:00:00',"%Y-%m-%d %H:%M:%S"))
time_q4 <- as.POSIXct(strptime('2016-04-01 09:00:00',"%Y-%m-%d %H:%M:%S"))
time_q5 <- as.POSIXct(strptime('2016-04-01 12:00:00',"%Y-%m-%d %H:%M:%S"))
time_q6 <- as.POSIXct(strptime('2016-04-01 15:00:00',"%Y-%m-%d %H:%M:%S"))
time_q7 <- as.POSIXct(strptime('2016-04-01 18:00:00',"%Y-%m-%d %H:%M:%S"))
time_q8 <- as.POSIXct(strptime('2016-04-01 21:00:00',"%Y-%m-%d %H:%M:%S"))


train$time_q <- difftime(train$Pickup_time,time_q1, units = "hours")


lr7 <- lm(trip_duration ~ haver + passenger_count + wd + month , data=train)
summary(lr7)




#numeric_train <- data.frame(train$passenger_count,train$trip_duration,train$haver)
#cor(numeric_train, method="pearson")
#plot(x=numeric_train$train.trip_duration, y= numeric_train$train.haver, type="h")

#d1 <- density(numeric_train$train.trip_duration, from=0, to=2200)
#plot(d1)
#boxplot(numeric_train$train.trip_duration)

#                       train.passenger_count train.trip_duration train.haver
#train.passenger_count           1.000000000         0.008470988  0.01030558
#train.trip_duration             0.008470988         1.000000000  0.09477678
#train.haver                     0.010305577         0.094776781  1.00000000