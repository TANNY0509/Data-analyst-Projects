#loading packages
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(ggcorrplot)

#import data files with readr
dailyActivity_merged <-   read_csv("D:/Download/Case_study/Bellbeats/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")

dailyActivity_merged2 <- read_csv("D:/Download/Case_study/Bellbeats/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

sleepDay_merged <- read_csv("D:/Download/Case_study/Bellbeats/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

#merge 2 daily activity dataset as one
daily_activity <- union(dailyActivity_merged, dailyActivity_merged2)

#Check both datasets with colnames()
colnames(daily_activity)
colnames(sleepDay_merged)

#fixing column names with clean_names() function
daily_activity <- clean_names(daily_activity)
sleepDay_merged <- clean_names(sleepDay_merged)

skim(daily_activity)
skim(sleepDay_merged)

#To find duplicates
duplicate_rows_activity <- daily_activity[duplicated(daily_activity[c("id", "activity_date")]), ]
print(duplicate_rows_activity)

duplicate_rows_sleep <- sleepDay_merged[duplicated(sleepDay_merged[c("id", "sleep_day")]), ]
print(duplicate_rows_sleep)

#To remove duplicates
daily_activity <- daily_activity[!duplicated(daily_activity[c("id", "activity_date")]), ]
sleepDay_merged <- sleepDay_merged[!duplicated(sleepDay_merged[c("id", "sleep_day")]), ]

#Convert char to date
daily_activity$activity_date <- as.Date(daily_activity$activity_date, format = "%m/%d/%Y")
sleepDay_merged$sleep_day <- as.Date(sleepDay_merged$sleep_day, format = "%m/%d/%Y")

#Verify after data cleaning
str(daily_activity)
str(sleepDay_merged)

#save cleaned datasets
write.csv(daily_activity, "D:/Download/Case_study/Bellbeats/mturkfitbit_export_3.12.16-4.11.16/dailyActivity_cleaned.csv", row.names = FALSE)
write.csv(sleepDay_merged, "D:/Download/Case_study/Bellbeats/mturkfitbit_export_3.12.16-4.11.16/sleepDay_cleaned.csv", row.names = FALSE)


#Data Analysis begins
#summary of the dataset
summary(daily_activity)
summary(sleepDay_merged)



#plotting visualization for daily activities
df_act <- daily_activity
df_act$weekday <- weekdays(df_act$activity_date)
df_act$moderate_activity_distance <- df_act$moderately_active_distance + df_act$light_active_distance
df_act$moderate_activity_minutes <- df_act$fairly_active_minutes + df_act$lightly_active_minutes

#Frequency of each metric
ggplot(df_act) + 
  geom_histogram(aes(x = calories), fill = "lightblue", binwidth = 200, color = "black") + labs(title = "Distribution of Calories Lost", y = "Freq")
ggplot(df_act) + 
  geom_histogram(aes(x = total_steps), fill = "navy", binwidth = 1600, color = "white") + labs(title = "Distribution of Total Steps", y = "Freq", x = "Steps taken")
ggplot(df_act) + 
  geom_histogram(aes(x = total_distance), fill = "cyan", color = "black") + labs(title = "Distribution of Distance", y = "freq", x = "Total Distance")
ggplot(df_act) + 
  geom_histogram(aes(x = very_active_minutes), fill = "green", color = "black") + labs(title = "Distribution of very active minutes", y = "Freq", x = "Very active minutes")
ggplot(df_act) + 
  geom_histogram(aes(x = sedentary_minutes), fill = "blue", color = "black") + labs(title = "Distribution of Sedentary minutes", y = "Freq", x = "Sedentary active minutes")
ggplot(df_act) + 
  geom_histogram(aes(x = very_active_distance), fill = "purple", color = "black") + labs(title = "Distribution of very active distance", y = "Freq", x = "Very active distance")
ggplot(df_act) + 
  geom_histogram(aes(x = moderate_activity_distance), fill = "yellow", color = "black") + labs(title = "Distribution of moderate activity distance", y = "Freq", x = "Moderate activity distance")
ggplot(df_act) + 
  geom_histogram(aes(x = moderate_activity_minutes), fill = "red", color = "black") + labs(title = "Distribution of moderate activity minutes", y = "Freq", x = "Moderate activity minutes")



ggplot(df_act, aes(x = activity_date, y = total_steps )) + 
  geom_point() + labs(title = "Total steps per day", x = "Activity date", y = "No of steps")
ggplot(df_act, aes(x = activity_date, y = total_distance )) + 
  geom_point() + labs(title = "Total steps per day", x = "Activity date", y = "No of steps")




ggplot(df_act) + geom_col(aes(x = weekday, y = total_steps), fill = "navy") + 
  labs(title = "Daily Steps per Week", x = "", y = "No of Steps"  ) + theme( axis.text.x = element_text(angle = 45, vjust = 0.6))




# correlation matrix for both datasets
# calculate correlation coefficient measure 
cor_daily <- round(cor(daily_activity[,-c(1,2,5,6)]), 2) # remove date and Id along with unneeded distance measures
cor_sleep <- round(cor(sleepDay_merged[-1:-2]), 2)

#Using ggcorrplot to produce correlation coefficient matrix visualization
ggcorrplot(cor_daily ,title = "Correlogram - Daily Activities", legend.title = "Pearsons \n Corr", lab = TRUE, lab_size = 2.8, outline.color = "black")
ggcorrplot(cor_sleep ,title = "Correlogram - Total Sleep in a Day", legend.title = "Pearsons \n Corr", lab = TRUE, outline.color = "black")



#create new dataframe to make tile graph
df <-  daily_activity[c(1:3)]
df$day <- weekdays(df$activity_date)
df <- subset(df, select = -c(activity_date))

#Tile graph using ggplot2
ggplot(df, aes(x = day, y = factor(id), fill = total_steps)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033" , , name = "Avg steps") + 
  labs(title = "Avg steps by participants per week", x ="Weekdays", y ="Participants", )  

ggplot(df_act, aes(x = weekday, y = factor(id), fill = total_distance)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033", name = "Avg Distance")  + 
  labs(title = "Avg distance by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = very_active_distance)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033" , , name = "Avg V-active distance") + 
  labs(title = "Avg very active distance by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = sedentary_active_distance)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033", name = "Avg sedentary distance") + 
  labs(title = "Avg sedentary distance by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = very_active_minutes)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033", name = "Avg v-active minutes") + 
  labs(title = "Avg very active minutes by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = sedentary_minutes)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033", name = "Avg sendetary minutes") + 
  labs(title = "Avg sedentary minutes by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = moderate_activity_distance)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033" , , name = "Avg moderate \nactive distance") + 
  labs(title = "Avg moderate active distance by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = moderate_activity_minutes)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033", name = "Avg moderate \nactive minutes") + 
  labs(title = "Avg moderate active minutes by participants per week", x ="Weekdays", y ="Participants")

ggplot(df_act, aes(x = weekday, y = factor(id), fill = calories)) + geom_tile(color = "white") + 
  theme_minimal() +  scale_fill_gradient(low =    "lightblue" , high="#cc0033", name = "Avg calories") + 
  labs(title = "Avg calories by participants per week", x ="Weekdays", y ="Participants")



#plotting visualization for Daily Sleep 
sleepDay_merged$weekday <- weekdays(sleepDay_merged$sleep_day)

ggplot(sleepDay_merged, aes(x=total_sleep_records)) + geom_histogram(binwidth = 1, fill = "brown" , color = "black") + labs(title = "Distribution of Total Sleep Records" , y = "freq" , x = "Sleep Records")
ggplot(sleepDay_merged, aes(x=total_minutes_asleep)) + geom_histogram(fill = "purple" , color = "black") + labs(title = "Distribution of Total Minutes Asleep" , y = "freq" , x = "Minutes asleep")
ggplot(sleepDay_merged, aes(x=total_time_in_bed)) + geom_histogram(binwidth = 35, fill = "orange" , color = "black") + labs(title = "Distribution of Total Time in Bed" , y = "freq" , x = "Time in Bed")
