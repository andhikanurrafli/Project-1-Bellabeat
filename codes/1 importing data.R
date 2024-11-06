# setting the environment
install.packages("tidyverse")
library(tidyverse)

# importing the data

daily_Activity <- read.csv("dailyActivity_merged.csv")
heartrate_seconds <- read.csv("heartrate_seconds_merged.csv")
hourly_Calorires <- read.csv("hourlyCalories_merged.csv")
hourly_Intensities <- read.csv("hourlyIntensities_merged.csv")
hourly_Steps <- read.csv("hourlySteps_merged.csv")
minute_Calories_Narrow <- read.csv("minuteCaloriesNarrow_merged.csv")
minute_Intensities_Narrow <- read.csv("minuteIntensitiesNarrow_merged.csv")
minute_METs_Narrow <- read.csv("minuteMETsNarrow_merged.csv")
minute_sleep <- read.csv("minuteSleep_merged.csv")
minute_Steps_Narrow <- read.csv("minuteStepsNarrow_merged.csv")
weight_Log_Info <- read.csv("weightLogInfo_merged.csv")

#take a peek for the data with 

head()
glimpse()
