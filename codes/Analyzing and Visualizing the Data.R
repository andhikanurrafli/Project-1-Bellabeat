# we are gonna check the average usage of the tracker and which day has the most usage

trackerDistanceMEAN <- combined_activity_dataCLEAN %>% summarize(mean(TrackerDistance))

# we need to know which day has the most activity

ggplot(data = combined_activity_dataCLEAN) + 
  geom_bar(mapping = aes(x = DayOfWeek, fill = DayOfWeek)) +
  theme(
    axis.text.x = 
          element_text(
            angle = 35
            ))

# wait that is not in ordered, We need to make it look good so

# Ensure 'DayOfWeek' is ordered correctly
combined_activity_dataCLEAN$DayOfWeek <- factor(
  combined_activity_dataCLEAN$DayOfWeek, 
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

# Create the plot
ggplot(data = combined_activity_dataCLEAN) + 
  geom_bar(mapping = aes(x = DayOfWeek, fill = DayOfWeek)) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  ) +
  labs(title = "Activity by Day of the Week", x = "Day of Week", y = "Count")

# We see that saturday is the highest activity
# to prove that I am gonna put a red line on the visual on tuesday
# but first I need to create a dataset to count the tuesday activity
tuesday_count <- combined_activity_dataCLEAN %>%
  filter(DayOfWeek == "Tuesday") %>%
  nrow()

# now we plot
ggplot(data = combined_activity_dataCLEAN) + 
  geom_bar(mapping = aes(x = DayOfWeek, fill = DayOfWeek)) +
  geom_hline(yintercept = tuesday_count, color = "red", linetype = "dashed") +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  ) +
  labs(title = "Activity by Day of the Week", x = "Day of Week", y = "Count")

# see Saturday is the highest

# now that we know saturday has the most acitivy
# we are gonna count how many tracker distance that our user have only on saturday

trackerdistanceSATURDAYMEAN <- combined_activity_dataCLEAN %>% filter( DayOfWeek == "Saturday") %>% 
  summarize(totaltrackerdistance = mean(TrackerDistance))

# now that we know the average miles of trackerdistance in saturday we now compare it to the whole data average

TD_merged <- cbind(trackerDistanceMEAN,trackerdistanceSATURDAYMEAN)

# now we know that saturday really has slightly has higher usage, that means we are gonna leave it and draw a conclusion to make a decision



# secara kelompok


# We need to know first how many total distance compare to the calories burns

combined_activity_dataCAL <- combined_activity_dataCLEAN %>% 
  mutate(CaloriesPerDistance = TotalCalories / TotalDistance)

# clean them!

combined_activity_dataCALCLEAN <- combined_activity_dataCAL %>%
  filter(is.finite(CaloriesPerDistance))

# we now analyze the data

daily_trends <- combined_activity_dataCAL %>% 
  group_by(ActivityDate) %>% 
  summarize(AverageCaloriesPerDistance = mean(CaloriesPerDistance, na.rm = TRUE))

# wow there is an infinity value in our dataset we need to clean it!

daily_trendsCLEAN <- daily_trends %>%
  mutate(AverageCaloriesPerDistance = ifelse(is.infinite(
    AverageCaloriesPerDistance), NA, AverageCaloriesPerDistance))

# now let's try to visualize it

ggplot(daily_trendsCLEAN, aes(x = ActivityDate, y = AverageCaloriesPerDistance)) +
  geom_line() +
  labs(title = "Average Calories Burned per Distance by Day",
       x = "Date",
       y = "Calories Burned per Distance")



# secara individu


# we need to know how much each individual burn their calories so that we can
# encourage them to do more exercise!

user_avg_calories_per_day <- combined_activity_dataCALCLEAN %>%
  group_by(Id, ActivityDate) %>%
  summarize(AverageCaloriesPerDistance = mean(CaloriesPerDistance, 
                                              na.rm = TRUE))
# clean them one more time
user_avg_calories_per_dayCLEAN <- user_avg_calories_per_day %>%
  filter(!is.nan(AverageCaloriesPerDistance))

# with that much of users i will take 3 of them to visualize the average calories burns by them

# first I want to group them in 1 dataset

specific_users <- user_avg_calories_per_dayCLEAN %>%
  filter(Id %in% c(1503960366, 1624580081, 1844505072)) 

# now let's see the visual

ggplot(specific_users, aes(x = ActivityDate, y = AverageCaloriesPerDistance, color = factor(Id))) +
  geom_line() + 
  geom_point() + 
  labs(
    title = "Average Calories Burned per Distance for 3 Users",
    x = "Activity Date",
    y = "Average Calories per Distance",
    color = "Id"
  ) +
  facet_wrap(~Id) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)  # Tilt the x-axis labels by 35 degrees
  )

# wow user 1624580081 really have exercise more






