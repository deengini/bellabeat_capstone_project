# install and load packages
library(ggplot2)

# load the cleaned dataset 
activity_df6 <- read.csv("cleaned_data.csv")
head(activity_df6)

# create a bar chart of total steps by day of week 
ggplot(activity_df6, aes(x = week_days, y = total_steps, fill = week_days)) +
  geom_bar(stat = "identity") +
  labs(x = "Days of the Week", y = "Total Steps", title = "Total Steps by Day of the Week",
       subtitle = "Sample of Fitbit Tracking Data from March 2016 to May 2016",
       caption = "Data source: Amazon Mechanical Turk") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 8))

# create a scatter plot chart of total steps by calories
ggplot(activity_df6, aes(x = total_steps, y = calories)) +
  geom_point(color = "steel blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Total Steps", y = "Calories", title = "Total Steps vs. Calories Burned",
       subtitle = "Sample of Fitbit Tracking Data from March 2016 to May 2016",
       caption = "Data source: Amazon Mechanical Turk") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 8)) +
  annotate("text", x = 5000, y = 200, label = "Note: Outlier data point at above 35,000 steps",
           size = 4, color = "black", hjust = 0)

# create a scatter plot chart of total distance by calories
ggplot(activity_df6, aes(x = total_dist, y = calories)) +
  geom_point(color = "steel blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  
  # add chart title, subtitle and caption
  labs(x = "Total Distance", y = "Calories", title = "Total Distance vs. Calories Burned",
       subtitle = "Sample of Fitbit Tracking Data from March 2016 to May 2016",
       caption = "Data source: Amazon Mechanical Turk") +
 
  #customize chart theme 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 8))

# create a pie chart of the percentage of mins spent per activity

#calculate total minutes for each category 
total_very_active <- sum(activity_df$very_active_mins)
total_fairly_active <- sum(activity_df$fairly_active_mins)
total_lightly_active <- sum(activity_df$lightly_active_mins)
total_sedentary <- sum(activity_df$sedentary_mins)
total_mins <- sum(activity_df$total_mins)

# calculate the percentage for each activity
perc_very_mins <- round((total_very_active / total_mins * 100), 1)
perc_fair_mins <- round((total_fairly_active / total_mins * 100), 1)
perc_light_mins <- round((total_lightly_active / total_mins * 100), 1)
perc_sed_mins <- round((total_sedentary / total_mins * 100), 1)

library(ggplot2)

# Define the data
activity_levels <- c("Very Active", "Fairly Active", "Lightly Active", "Sedentary")
percentages <- c(perc_very_mins, perc_fair_mins, perc_light_mins, perc_sed_mins)

# Create a data frame
percent_df <- data.frame(activity_levels, percentages)

# Create a pie chart using ggplot2
ggplot(percent_df, aes(x = "", y = percentages, fill = activity_levels)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  
  # add percentage labels using geom_text
  geom_text(aes(label = paste0(percentages)),
            position = position_stack(vjust = 0.5)) +

  # add chart title, subtitle and caption            
  labs(title = "Percentage of Time spent per Activity",
       subtitle = "Sample of Fitbit Tracking Data from March 2016 to May 2016",
       caption = "Data source: Amazon Mechanical Turk") +
 
  #customize chart theme 
  theme_void() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12)) +
    
  #customize fill colors  
  scale_fill_manual(values = c("yellow", "orange", "red", "green"))

# Alternative way of creating pie charts
pie(percentages, 
    main = "Percentage of Time spent per Activity",
    col = c("green", "yellow", "orange", "red"),
    labels = c("Very Active", "Fairly Active", "Lightly Active", "Sedentary"),
    cex = 0.8)
mtext("Sample of Fitbit Tracking Data from March 2016 to May 2016", side = 3, line = -1, cex = 0.8)
title("Data was collected via Amazon Mechanical Turk", cex.lab = 0.8, line = -14, font = 3)







