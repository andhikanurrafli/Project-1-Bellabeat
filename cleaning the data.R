# changing ActivityDate on daily_Activity as date

daily_Activity$ActivityDate <- as.Date(daily_Activity$ActivityDate, 
                                       format = "%m/%d/%Y")


# classifying data by Id, ActivityDate, Total Distance, Tracker Distance and calories


combined_activity_data <- daily_Activity %>%
  group_by(Id, ActivityDate) %>%
  summarise(
    TotalSteps = sum(TotalSteps),
    TotalCalories = sum(Calories),
    TrackerDistance = sum(TrackerDistance),
    TotalDistance = sum(TotalDistance),
    .groups = 'drop'
  )

# we now need to find the time span of the user's usage

start_date <- min(combined_activity_data$ActivityDate)
end_date <- max(combined_activity_data$ActivityDate)

end_date - start_date


# next we need to determine which day has the most activity each day

combined_activity_data <- combined_activity_data %>% 
  mutate(DayOfWeek = weekdays(ActivityDate))

# we need to clean it  one more time

combined_activity_dataCLEAN <- combined_activity_data %>%
  filter(apply(combined_activity_data, 1, function(x) all(!is.na(x) & x != 0 & x !=0.00)))

glimpse(combined_activity_data)
head(combined_activity_data)