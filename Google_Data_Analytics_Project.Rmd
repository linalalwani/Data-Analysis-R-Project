---
title: "Google Data Analytics Capstone"
author: "Lina Lalwani"
date: "1/4/2022"
output: html_notebook
---

# Case Study: Marketing Analyst Growth Strategy for Cyclistic

## Introduction
 
This data and case study comes from the Google Data Analytics Professional Certificate Course. I will be showing my results and analysis by following Google's six steps of the data analysis process - ask, prepare, process, analyze, share and act. 

## Step 1: Ask
### Context

Cyclistic - a bike-share company in Chicago with more than 5,800 bicycles and 600 docking stations - is working on maximizing their number of annual memberships for company growth. 

Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders.

### Business Task

To understand how annual members and casual riders differ with a goal of converting casual riders into annual members.

### Key Stakeholders

The Key Stakeholders in this project are director of marketing, Cyclistic's executive team.

## Step 2: Prepare

This data has been made available by Motivate International Inc. under this license.) This is public data that you can use to explore how different customer types are using Cyclistic bikes. Data-privacy regulations shield riders’ personally identifiable information.

The data contain information of trip duration and gps coordinates. Datasets dated from 2015 to 2021 are organized by quarter, and April 2020 and to November of 2021 are organized by month.For the purposes of this analysis and to retain relevancy, I will be forgoing use of more outdated data to use the available monthly trip data. 



```{r echo=TRUE}

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)

#Begin by importing all of the datasets
trip_202004 <- read_csv("~/Desktop/trips bike/202004-divvy-tripdata.csv")
trip_202005 <- read_csv("~/Desktop/trips bike/202005-divvy-tripdata.csv")
trip_202006 <- read_csv("~/Desktop/trips bike/202006-divvy-tripdata.csv")
trip_202007 <- read_csv("~/Desktop/trips bike/202007-divvy-tripdata.csv")
trip_202008 <- read_csv("~/Desktop/trips bike/202008-divvy-tripdata.csv")
trip_202009 <- read_csv("~/Desktop/trips bike/202009-divvy-tripdata.csv")
trip_202010 <- read_csv("~/Desktop/trips bike/202010-divvy-tripdata.csv")
trip_202011 <- read_csv("~/Desktop/trips bike/202011-divvy-tripdata.csv")
trip_202012 <- read_csv("~/Desktop/trips bike/202012-divvy-tripdata.csv")
trip_202101 <- read_csv("~/Desktop/trips bike/202101-divvy-tripdata.csv")
trip_202102 <- read_csv("~/Desktop/trips bike/202102-divvy-tripdata.csv")
trip_202103 <- read_csv("~/Desktop/trips bike/202103-divvy-tripdata.csv")
trip_202104 <- read_csv("~/Desktop/trips bike/202104-divvy-tripdata.csv")
trip_202105 <- read_csv("~/Desktop/trips bike/202105-divvy-tripdata.csv")
trip_202106 <- read_csv("~/Desktop/trips bike/202106-divvy-tripdata.csv")
trip_202107 <- read_csv("~/Desktop/trips bike/202107-divvy-tripdata.csv")
trip_202108 <- read_csv("~/Desktop/trips bike/202108-divvy-tripdata.csv")
trip_202109 <- read_csv("~/Desktop/trips bike/202109-divvy-tripdata.csv")
trip_202110 <- read_csv("~/Desktop/trips bike/202110-divvy-tripdata.csv")
trip_202111 <- read_csv("~/Desktop/trips bike/202111-divvy-tripdata.csv")
```


```{r}
#Then, combine all of the datasets into one

trip <- rbind(trip_202004,
              trip_202005,
              trip_202006,
              trip_202007,
              trip_202008,
              trip_202009,
              trip_202010,
              trip_202011,
              trip_202012,
              trip_202101,
              trip_202102,
              trip_202103,
              trip_202104,                
              trip_202105,
              trip_202106,
              trip_202107,
              trip_202108,
              trip_202109,
              trip_202110)


```

Now that the data has been combined into one table, I will analyze different characteristics of the dataset.

```{r}
nrow(trip) #check number of rows
dim(trip) #check number of rows + columns
head(trip) #check first few lines of data

#key check
summary(trip) #summary of data

```

## Step 3: Process

With the appropriate columns created, the data is ready to be cleaned.


```{r}
#I have removed any rows with missing values, into a new datafram trip_clean

colSums(is.na(trip))
trip_clean <- drop_na(trip)
trip_clean <- trip_clean %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))

#This removed close to a mission rows with incomplete data

#Data with a larger greater than time than ended than time will next be removed

# Data with started_at greater than ended_at will be removed. I will use the original date-time values to make sure times are also applied

trip_clean <- trip_clean %>% 
  filter(trip_clean$started_at < trip_clean$ended_at)

# As each ride has a single, unique ride ID, rows with a duplicate unique ID should also be removed. In the case that there are duplicates with varying rows, the most recent or last value should be kept.

trip_clean <- trip_clean %>% distinct(ride_id, .keep_all= TRUE, fromLast=T)

```

I will begin to add the appropriate columns for later analysis. 

```{r}

#Gone ahead and added the relevant information to separate start and end days from their date time format. 

trip_clean$start_date <- as.Date(trip_clean$started_at)
trip_clean$end_date <- as.Date(trip_clean$ended_at)

#Once I completed the above, I could sift out by day, month, week, and year for each row.

trip_clean$day <- format(as.Date(trip_clean$start_date), "%d")
trip_clean$month <- format(as.Date(trip_clean$start_date), "%m")
trip_clean$year <- format(as.Date(trip_clean$start_date), "%Y")
trip_clean$weekday <- format(as.Date(trip_clean$start_date), "%A")


#I will add numerical values to capture the time duration for each ride
trip_clean$ride_length <- as.numeric(difftime(trip_clean$ended_at,trip_clean$started_at))

#I will then remove any rides that are equal to or less than 0

trip_clean <- filter(trip_clean, ride_length >= 0)

str(trip_clean)

#make sure variables do not have values outside of member or casual
trip_clean <- trip_clean %>% filter(member_casual %in% "member" | member_casual %in% "casual") 

```

## Step 4: Analyze


Now that the data has been thoroughly checked and cleaned, it is ready to begin deciphering the key differences between the two groups

### Group by Members and Casual

I started by getting the mean, median, minimum, and maximum numbers of all rides. I then obtained these key stats for annual members versus casuals. - as well as the total count for each of these two groups.

```{r}

#get the mean, median, min, max
summary(trip_clean$ride_length)

#get the mean, median, min, max, and total counts separated by each group
trip_clean %>% 
  group_by(member_casual) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n())

```
From the information above:

* The mean and median length of rides are higher for casuals than members.
* The number of rides taken by members are higher than casuals. 
  * Unfortunately, a limitation in this dataset is we do not know the total number of members versus users. Therefore, we cannot determine whether there are more members than users, or simply a higher number of usage across the available members. 


### Group by Days of the Week

From the information above, I wanted to see how this would be impacted when separating each category by weekday. Once I performed this analysis, I checked to see if trends were impacted by members vs casuals.

```{r}
#to group by weekday order, added custom arrange() for cleaner view
trip_clean %>% 
  group_by(weekday) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n()) %>% 
  arrange(factor(weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

#this table prioritizes arrange by day of the week
trip_clean %>% 
  group_by(member_casual, weekday) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n()) %>%
  arrange(factor(weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), member_casual)


#this table prioritizes arrange by member vs user
trip_clean %>% 
  group_by(member_casual, weekday) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n()) %>%
  arrange(member_casual, factor(weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
```
From the information above:

* Weekends show a higher median and mean ride length, as well as a higher number of rides for both groups.
* This higher ride length remains true for both groups when viewed separately
* While casuals have a higher count of rides on weekends, this trend is not seen for members - noting the lowest total number rides out of all the days of the week falls on Sunday. 


### Group by Month

```{r}
trip_clean %>% 
  group_by(month) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n())

#this table groups by casuals and members
trip_clean %>% 
  group_by(member_casual, month) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n())

```

From the information above:

* Peak ride length across both groups is from April to June, with a higher number of rides from July to August
*These trends remain the same across both casuals and members.

### Group by Day in the Month

We performed a quick check to see if the day in the month impacted the ride length or number of rides that casuals and/or members were taking.

```{r}
colnames(trip_clean)

trip_clean %>% 
  group_by(day) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n())

#this table groups by casuals and members
trip_clean %>% 
  group_by(member_casual, day) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n())


write.csv(trip_clean, path)

```
The day of the month, as expected, did not make much of a difference in both the length of rides as well as the count of rides.

### Findings

From the above analysis, we have found the following:

* Casuals have average ride times that are longer than members.
* Weekends show a higher median and mean ride length for both groups.
* Casuals have a higher percentage of total rides on weekends, but members have a more even split with a drop on Sundays.
* Peak ride length across both groups is from April to June, with a higher number of rides from July to August


## Share

To highlight these findings, I have created the following graphs.

```{r}

#general members vs casuals plot
trip_clean %>% 
  group_by(member_casual) %>% 
  summarize(median(ride_length), mean(ride_length), min(ride_length), max(ride_length), count = n()) %>% 
  ggplot(aes(x=member_casual, y=count)) +
  geom_bar(stat='identity', position = "dodge") + 
  labs(title = "Total Rides of Member and Casual Numbers", x = "Member or Casual" , y = "Number of Rides", fill = "Member - Casual")

trip_clean %>% 
  group_by(member_casual) %>% 
  summarize(median(ride_length), mean = mean(ride_length), min(ride_length), max(ride_length), count = n()) %>% 
  ggplot(aes(x=member_casual, y=mean)) +
  geom_bar(stat='identity', position = "dodge") + 
  labs(title = "Average Length of Rides of Member and Casual Numbers", x = "Member or Casual" , y = "Length of Ride", fill = "Member - Casual")



#plot for weekdays
trip_clean %>% 
  group_by(member_casual, weekday) %>% 
  summarize(count = n(), mean(ride_length)) %>% 
  arrange(member_casual, count)  %>%
  ggplot(aes(x=weekday, y=count, fill = member_casual)) +
  geom_bar(stat='identity', position = "dodge") + 
  labs(title = "Total Rides of Member and Casual Number of rides by Weekday", x = "Day of Week", y = "Number of Rides", fill = "Member - Casual")

trip_clean %>% 
  group_by(member_casual, weekday) %>% 
  summarize(count = n(), ride = mean(ride_length)) %>% 
  arrange(member_casual, count)  %>%
  ggplot(aes(x=weekday, y=ride, fill = member_casual)) +
  geom_bar(stat='identity', position = "dodge") + 
  labs(title = "Average Ride Length of Member and Casual Rides by Weekday", x = "Day of Week", y = "Length of Ride", fill = "Member - Casual")

#plot for months
trip_clean %>% 
  group_by(member_casual, month) %>% 
  summarize(count = n(), ride = mean(ride_length)) %>% 
  ggplot(aes(x=month, y=count, fill = member_casual)) +
  geom_bar(stat='identity', position = "dodge") + 
  labs(title = "Total Rides of Member and Casual Number of rides by Month", x = "Month", y = "Number of Rides", fill = "Member - Casual")


trip_clean %>% 
  group_by(member_casual, month) %>% 
  summarize(count = n(), ride = mean(ride_length)) %>% 
  ggplot(aes(x=month, y=ride, fill = member_casual)) +
  geom_bar(stat='identity', position = "dodge") + 
  labs(title = "Average Ride Length of Member and Casual Number of rides by Month", x = "Month", y = "Length of Ride", fill = "Member - Casual")
```



To expand on these findings, I would recommend requesting additional information on number of casuals versus members, as well as more demographic information about both of these groups. I would also request information regarding 

## Act 

From the above findings, my tops recommendations to the marketing agency would be the following:

* Provide weekend membership specials to casuals
* Encourage membership conversion through special deals on ridership length in April to June (i.e. new members get half off their existing ride) and count from July to August (i.e. members who sign on get free rides for the rest of the month)
* Share clear cost saving benefits of membership usage to specials, encouraging them to get an annual membership and save over renting for each trip. A "You could be saving X dollars!" could do the trick here

