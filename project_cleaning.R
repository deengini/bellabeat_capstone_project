# install packages and load environment 
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(lubridate)

# import daily activity data
activity_df <- read_csv("daily_activity_merged.csv")
head(activity_df)

# check for missing data 
missing_data <-is.na(activity_df)

# count the number of missing values in each column 
num_null <- apply(missing_data, 2, sum)
print(num_null)
# There are no missing data 

# Create columns for total time spent in hours and minutes
activity_df2 <- activity_df %>%
  mutate(total_mins = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes) %>% 
  mutate(total_hours = total_mins/60)


# check datatype of columns 
str(activity_df2)
head(activity_df2)

# change datatype of activity date column from str to date type 
activity_df2$ActivityDate <- as.Date(activity_df2$ActivityDate, format = "%m/%d/%Y")

str(activity_df2)

# extract name of days from date column as activity days
# extract month from date column as activity months
activity_df3 <- activity_df2 %>%
  mutate(activity_days = weekdays(ActivityDate)) %>% 
  mutate(activity_months = month(ActivityDate))

# convert month to month names
activity_df3$activity_months <- month.name[activity_df3$activity_months]

str(activity_df3)

# rearrange the columns of the dataset
activity_df4 <- select(activity_df3, "Id", "ActivityDate", "activity_months", "activity_days", "TotalSteps", "TotalDistance", "TrackerDistance", "LoggedActivitiesDistance", "VeryActiveDistance",
                       "ModeratelyActiveDistance", "LightActiveDistance", "SedentaryActiveDistance", "VeryActiveMinutes", "FairlyActiveMinutes", 
                       "LightlyActiveMinutes", "SedentaryMinutes", "total_mins", "total_hours", "Calories")

# rename all the columns in the dataset
#check the names
names(activity_df4)

# rename with dplyr
activity_df5 <- rename(activity_df4, "id" = "Id", "date" = "ActivityDate", "months" = "activity_months", "week_days" = "activity_days", "total_steps" = "TotalSteps", 
                      "total_dist" = "TotalDistance", "track_dist" = "TrackerDistance", "logged_dist" = "LoggedActivitiesDistance", "very_active_dist" = "VeryActiveDistance",
                      "moderate_active_dist" = "ModeratelyActiveDistance", "light_active_dist" = "LightActiveDistance", 
                      "sedentary_active_dist" = "SedentaryActiveDistance", "very_active_mins" = "VeryActiveMinutes", 
                      "fairly_active_mins" = "FairlyActiveMinutes", "lightly_active_mins" = "LightlyActiveMinutes", 
                      "sedentary_mins" = "SedentaryMinutes", "total_mins" = "total_mins", 
                      "total_hours" = "total_hours", "calories" = "Calories")

# make sure sure the column names are all unique and consistent
activity_df6 <- clean_names(activity_df5)

# there should be 30 participants in the survey so there should be 30 unique ids
unique(activity_df6$id)
length(unique(activity_df6$id))
# results shows 33 but since there no names matched to the data then we assume that all the users remained the same

# find summary statistics of the data
activity_summary <- data.frame(summary(activity_df6))
activity_summary

# create a box plot to identify outliers in total_steps
boxplot(activity_df6$total_steps)

# export cleaned data 
write.csv(activity_df6, "cleaned_data.csv", row.names = FALSE)
