
# Bellabeat Fitness Data Analysis

# Load required packages

  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(skimr)
  library(here)
# Data Import and Cleaning ------------------------------------------------

# Imported datasets 
daily_activity <- read_csv("data/dailyActivity_merged.csv", show_col_types = FALSE)  
sleep_data <- read_csv("data/sleepDay_merged.csv", show_col_types = FALSE)           
weight_data <- read_csv("data/weightLogInfo_merged.csv", show_col_types = FALSE)

# Standardized date formats and clean column names
daily_activity <- daily_activity %>%
  clean_names() %>%
  rename(date = activity_date) %>%  
  mutate(date = mdy(date))

sleep_data <- sleep_data %>%
  clean_names() %>%
  rename(date = sleep_day) %>%  
  mutate(date = mdy_hms(date)) %>% 
  mutate(date = as.Date(date))

weight_data <- weight_data %>%
  clean_names() %>%
  rename(date = date) %>%  
  mutate(date = mdy_hms(date)) %>% 
  mutate(date = as.Date(date))

# Merged datasets
merged_activity_sleep <- daily_activity %>%
  left_join(sleep_data, by = c("id", "date"))

merged_all_data <- merged_activity_sleep %>% 
  left_join(weight_data, by = c("id", "date"))

# Data Exploration -------------------------------------------------------

# Checked structure and summary
glimpse(merged_all_data)
summary(merged_all_data)

# Checked unique participants
cat("Unique participants in activity data:", n_distinct(daily_activity$id), "\n")
cat("Unique participants in sleep data:", n_distinct(sleep_data$id), "\n")
cat("Unique participants in weight data:", n_distinct(weight_data$id), "\n")
cat("Unique participants in merged data:", n_distinct(merged_all_data$id), "\n")

# Data Cleaning ----------------------------------------------------------

# Removed duplicates
no_dup_merged_data <- distinct(merged_all_data)

# Created new variables
clean_data_transformed <- no_dup_merged_data %>%
  mutate(
    total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes,
    weekday = weekdays(date)
  ) %>%
  filter(total_active_minutes + sedentary_minutes <= 1440)

# Exported cleaned data
write_csv(clean_data_transformed, "data/cleaned_fitbit_data.csv")

# Analysis and Visualization ----------------------------------------------

# Load cleaned data
df <- read_csv("data/cleaned_fitbit_data.csv", show_col_types = FALSE)

# Steps Analysis
summary(df$total_steps)
percent_10k <- mean(df$total_steps >= 10000, na.rm = TRUE) * 100

# Weekly step pattern
weekday_steps <- df %>% 
  group_by(weekday) %>% 
  summarize(total_steps = mean(total_steps)) %>% 
  mutate(weekday = factor(weekday,
                levels = c("Monday", "Tuesday", "Wednesday",
                "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE))

# Plot weekly steps
ggplot(weekday_steps, aes(x = weekday, y = total_steps, fill = weekday)) +
  geom_col(alpha = 0.5, show.legend = FALSE) +
  labs(title = "Total Steps per Weekday",
       x = NULL,
       y = "Total Steps") +
  geom_text(aes(label = round(total_steps)), vjust = -0.5, color = "black", size = 3) + 
  theme_minimal() +
  theme(plot.title = element_text(color = "grey40", size = 14, face = "bold"),
        axis.title.y = element_text(color = "grey"))

# Activity Levels Analysis
activity_min_avg <- df %>% 
  summarise(
    avg_very_active_mins = mean(very_active_minutes),
    avg_fairly_active_mins = mean(fairly_active_minutes),
    avg_light_active_mins = mean(lightly_active_minutes)
  )

df_activity_long <- pivot_longer(activity_min_avg, everything(), 
                               names_to = "activity_type", 
                               values_to = "avg_minutes")

# Plot ctivity levels
ggplot(data = df_activity_long, 
       aes(x = activity_type, y = avg_minutes, fill = activity_type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste(round(avg_minutes, 1), "mins")), 
            vjust = -0.5, color = "black", size = 3.5) +
  scale_y_continuous(name = "Minutes", breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
  scale_x_discrete(labels = c("avg_very_active_mins" = "Very Active",
                             "avg_fairly_active_mins" = "Fairly Active",
                             "avg_light_active_mins" = "Light Active")) +
  labs(x = NULL, y = "Average Minutes per Day", title = "Daily Physical Activity") +  
  theme_minimal()

# Sleep Analysis Histogram
sleep_data <- df %>%  
  mutate(total_hours_asleep = total_minutes_asleep/60)

# Plot sleep distribution
ggplot(data = sleep_data) +
  geom_histogram(aes(x = total_hours_asleep),
                 color = "orange", fill = "pink",
                 binwidth = 2, na.rm = TRUE) +
  geom_vline(xintercept = 7, linetype = "dashed",
             color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0, 13), breaks = seq(0, 13, by = 2)) +
  scale_y_continuous(name = "Count of Reports", limits = c(0, 200)) +
  labs(x = "Total Hours Asleep", title = "Sleep Duration Distribution") +
  theme_minimal()
