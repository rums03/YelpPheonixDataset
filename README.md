# YelpPheonixDataset
##################################################################################
#=====================YELP DATASET ANALYSIS FOR SLALOM=====================
#=Ramya Sreekumar==========================================================
#==========================================================================
##################################################################################

#Set Working directory. Make sure the yelpSlalom csv file is present here
setwd("C:/Users/ramya/Desktop/R Slalom")

#Package installations. USe just for the first time, load the packages after
install.packages("car")
install.packages("rgl")
install.packages("corrplot")
install.packages("knitr")
install.packages("caret")
install.packages("readr")
install.packages("stringr")
install.packages("mice")
install.packages("e1071")

#Load Packages
library(car)
library(corrplot)
library(knitr)
library(caret)
library(readr)
library(stringr)
library(mice)

#Loading all required packages for OLR
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(e1071)

#========Loading Data into R======================================================

#Load the raw data into R, 1st row is a header and replace all blanks with NA
MyData <- read.csv(file="yelpdata1.csv", header=TRUE, sep=",", na.strings = c("","NA"), dec = ".")
#View loaded data, ensure proper load
View(MyData)
#Check number of records in the full dataset
nrow(MyData)
#Understand the structure of the dataset
str(MyData)
#Look at the first 6 records to understand dataset and ensure proper load
head(MyData)


#Dummy Variables to be created for all categorical variables. One way of doing this is in R is by converting them to factor variables
MyData$Business...Accepts.Credit.Cards.f = factor(MyData$Business...Accepts.Credit.Cards)
MyData$Business...Ages.Allowed.f = factor(MyData$Business...Ages.Allowed)
MyData$Business...Alcohol.f = factor(MyData$Business...Alcohol)
MyData$Business...Attire.f = factor(MyData$Business...Attire)
MyData$Business...BYOB.Corkage.f = factor(MyData$Business...BYOB.Corkage)
MyData$Business...BYOB.f = factor(MyData$Business...BYOB)
MyData$Business...By.Appointment.Only.f = factor(MyData$Business...By.Appointment.Only)
MyData$Business...Caters.f = factor(MyData$Business...Caters)
MyData$Business...Coat.Check.f = factor(MyData$Business...Coat.Check)
MyData$Business...Corkage.f = factor(MyData$Business...Corkage)
MyData$Business...Delivery.f = factor(MyData$Business...Delivery)
MyData$Business...Dietary.Restrictions.f = factor(MyData$Business...Dietary.Restrictions)
MyData$Business...Dogs.Allowed.f = factor(MyData$Business...Dogs.Allowed)
MyData$Business...Drive.Thru.f = factor(MyData$Business...Drive.Thru)
MyData$Business...Good.For.Dancing.f = factor(MyData$Business...Good.For.Dancing)
MyData$Business...Good.For.Groups.f = factor(MyData$Business...Good.For.Groups)
MyData$Business...Good.for.Kids.f = factor(MyData$Business...Good.for.Kids)
MyData$Business...Happy.Hour.f = factor(MyData$Business...Happy.Hour)
MyData$Business...Has.TV.f = factor(MyData$Business...Has.TV)
MyData$Business...Noise.Level.f = factor(MyData$Business...Noise.Level)
MyData$Business...Open.24.Hours.f = factor(MyData$Business...Open.24.Hours)
MyData$Business...Order.at.Counter.f = factor(MyData$Business...Order.at.Counter)
MyData$Business...Outdoor.Seating.f = factor(MyData$Business...Outdoor.Seating)
MyData$Business...Price.Range.f = factor(MyData$Business...Price.Range)
MyData$Business...Smoking.f = factor(MyData$Business...Smoking)
MyData$Business...Take.out.f= factor(MyData$Business...Take.out)
MyData$Business...Takes.Reservations.f = factor(MyData$Business...Takes.Reservations)
MyData$Business...Waiter.Service.f = factor(MyData$Business...Waiter.Service)
MyData$Business...Wheelchair.Accessible.f = factor(MyData$Business...Wheelchair.Accessible)
MyData$Business...Wi.Fi.f = factor(MyData$Business...Wi.Fi)
MyData$Business...City.f = factor(MyData$Business...City)
MyData$Business...Categories.f = factor(MyData$Business...Categories)
MyData$Business...Stars.f = factor(MyData$Business...Stars)
levels(MyData$Business...Stars.f)

#Subset the dataset into a smaller sample for quicker and easy processing on personal laptop
#Note: (proportion of records will be maintainted by R while using the createDataPartition function)
set.seed(123)
index <- createDataPartition(y=MyData$Business...Stars.f, p=.2,list = FALSE,times = 1)
sampledata <- MyData[index,]
nrow(sampledata)


# Training and Test Data splits created using the Caret package
set.seed(123)
index1 <-createDataPartition(y=sampledata$Business...Stars, p=.7, list=FALSE, times = 1)
# Training data split 70%
Train.data <- sampledata[index1,]
# Testing data split 30%
Test.data <- sampledata[-index1,]

#View number of records in training dataset
nrow(Train.data)
#View number of records in testing dataset
nrow(Test.data)

#View Train and Test datasets to ensure proper split
View(Train.data)
View(Test.data)

#============Multiple Linear Model : User Attributes====================================================
# What factors impact a User Average Rating. This can be used by the business to target users most likely to provide high ratings

#Run a Multiple Linear Regression Model to predict  average stars using User attributes 
#The other variables can be added to this as we scale up this model to a more powerful processing system
lmmod1 <- lm(User...Average.Stars ~ User...Compliments.Cool + 
               User...Review.Count+ 
               User...Compliments.Cute +
               User...Compliments.Funny +
               User...Compliments.Hot +
               User...Compliments.Photos +
               User...Fans, 
             data = Train.data)

#Check results of the LM model
summary(lmmod1)

#Use the LM training model to predict the test data results
lmodtest <- predict(lmmod1,Test.data)
lmodtest
postResample(lmodtest, Test.data$User...Average.Stars)

#======Using Ordinal Regression Model======Business Star Prediction based on Business Attributes===================
# What factors of a business impact the Average Business Ratings. This can be used by the business to improve its overall rating.
#Creating the OLR model for the training data using polr function
olr.trainB <- polr(Business...Stars.f ~ User...Average.Stars + Business...Review.Count + Business...Wi.Fi.f + Business...Wheelchair.Accessible.f
                   + Business...Has.TV.f + Business...Waiter.Service.f + Business...BYOB.f + Business...Take.out.f ,
                   data = Train.data, Hess = TRUE)

#See the results of training model
olr.trainB

#Use the trained model for predicting the stars of the test data
olr.testB <- predict(olr.trainB, Test.data)
#See the results of the test model
olr.testB

#Compare the results of the test predicted to the actual data
confusionMatrix(olr.testB,Test.data$Business...Stars.f)

#======= Accuracy Rate for Business Attributes : 57.81%
