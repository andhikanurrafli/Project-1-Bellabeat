Markdown Project 1: Bellabeat
================
Andhika Nurrafli Putra
2024-11-05

### Dataset

The data I use in this task is: [FitBit Fitness Tracker
Data](https://www.kaggle.com/datasets/arashnic/fitbit) (CCO:Public
Domain from [Möbius](https://www.kaggle.com/arashnic)).

### **Determining Bellabeat Business Task**

First step of data analyzing is determining the question of the
business. Or as we know the Business task. In this case Urška Sršen and
Sando Mur is establishing a smart product that monitoring health of a
person. Sršen know that this business has the potential to do more. So
we need to focus on the development of Bellabeat. In this case I already
determine the with:  
- *What are the usage patterns of Bellabeat smart devices?*  
- *What we can do to encourage them workout more?*  
We need to Know how to Improve the usage and broaden our products so
people will engage to our product.

### **Preparing the Data**

In this step I want to determine first problem that bellabeat has. First
we need to load the data

``` r
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

head(daily_Activity)
```

    ##           Id ActivityDate TotalSteps TotalDistance TrackerDistance
    ## 1 1503960366    3/25/2016      11004          7.11            7.11
    ## 2 1503960366    3/26/2016      17609         11.55           11.55
    ## 3 1503960366    3/27/2016      12736          8.53            8.53
    ## 4 1503960366    3/28/2016      13231          8.93            8.93
    ## 5 1503960366    3/29/2016      12041          7.85            7.85
    ## 6 1503960366    3/30/2016      10970          7.16            7.16
    ##   LoggedActivitiesDistance VeryActiveDistance ModeratelyActiveDistance
    ## 1                        0               2.57                     0.46
    ## 2                        0               6.92                     0.73
    ## 3                        0               4.66                     0.16
    ## 4                        0               3.19                     0.79
    ## 5                        0               2.16                     1.09
    ## 6                        0               2.36                     0.51
    ##   LightActiveDistance SedentaryActiveDistance VeryActiveMinutes
    ## 1                4.07                       0                33
    ## 2                3.91                       0                89
    ## 3                3.71                       0                56
    ## 4                4.95                       0                39
    ## 5                4.61                       0                28
    ## 6                4.29                       0                30
    ##   FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes Calories
    ## 1                  12                  205              804     1819
    ## 2                  17                  274              588     2154
    ## 3                   5                  268              605     1944
    ## 4                  20                  224             1080     1932
    ## 5                  28                  243              763     1886
    ## 6                  13                  223             1174     1820

``` r
glimpse(daily_Activity)
```

    ## Rows: 457
    ## Columns: 15
    ## $ Id                       <dbl> 1503960366, 1503960366, 1503960366, 150396036…
    ## $ ActivityDate             <chr> "3/25/2016", "3/26/2016", "3/27/2016", "3/28/…
    ## $ TotalSteps               <int> 11004, 17609, 12736, 13231, 12041, 10970, 122…
    ## $ TotalDistance            <dbl> 7.11, 11.55, 8.53, 8.93, 7.85, 7.16, 7.86, 7.…
    ## $ TrackerDistance          <dbl> 7.11, 11.55, 8.53, 8.93, 7.85, 7.16, 7.86, 7.…
    ## $ LoggedActivitiesDistance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveDistance       <dbl> 2.57, 6.92, 4.66, 3.19, 2.16, 2.36, 2.29, 3.3…
    ## $ ModeratelyActiveDistance <dbl> 0.46, 0.73, 0.16, 0.79, 1.09, 0.51, 0.49, 0.8…
    ## $ LightActiveDistance      <dbl> 4.07, 3.91, 3.71, 4.95, 4.61, 4.29, 5.04, 3.6…
    ## $ SedentaryActiveDistance  <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.0…
    ## $ VeryActiveMinutes        <int> 33, 89, 56, 39, 28, 30, 33, 47, 40, 15, 43, 3…
    ## $ FairlyActiveMinutes      <int> 12, 17, 5, 20, 28, 13, 12, 21, 11, 30, 18, 18…
    ## $ LightlyActiveMinutes     <int> 205, 274, 268, 224, 243, 223, 239, 200, 244, …
    ## $ SedentaryMinutes         <int> 804, 588, 605, 1080, 763, 1174, 820, 866, 636…
    ## $ Calories                 <int> 1819, 2154, 1944, 1932, 1886, 1820, 1889, 186…

### **Cleaning the data**

Cleaning the data helps analysis become easier, this way you can focus
on the analysis rather than seeing messy Datasets.

``` r
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
```

    ## Time difference of 31 days

``` r
# next we need to determine which day has the most activity each day

combined_activity_data <- combined_activity_data %>% 
  mutate(DayOfWeek = weekdays(ActivityDate))

# we need to clean it  one more time

combined_activity_dataCLEAN <- combined_activity_data %>%
  filter(apply(combined_activity_data, 1, function(x) all(!is.na(x) & x != 0 & x !=0.00)))


glimpse(combined_activity_data)
```

    ## Rows: 457
    ## Columns: 7
    ## $ Id              <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 150396…
    ## $ ActivityDate    <date> 2016-03-25, 2016-03-26, 2016-03-27, 2016-03-28, 2016-…
    ## $ TotalSteps      <int> 11004, 17609, 12736, 13231, 12041, 10970, 12256, 12262…
    ## $ TotalCalories   <int> 1819, 2154, 1944, 1932, 1886, 1820, 1889, 1868, 1843, …
    ## $ TrackerDistance <dbl> 7.11, 11.55, 8.53, 8.93, 7.85, 7.16, 7.86, 7.87, 7.25,…
    ## $ TotalDistance   <dbl> 7.11, 11.55, 8.53, 8.93, 7.85, 7.16, 7.86, 7.87, 7.25,…
    ## $ DayOfWeek       <chr> "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "…

``` r
head(combined_activity_data)
```

    ## # A tibble: 6 × 7
    ##           Id ActivityDate TotalSteps TotalCalories TrackerDistance TotalDistance
    ##        <dbl> <date>            <int>         <int>           <dbl>         <dbl>
    ## 1 1503960366 2016-03-25        11004          1819            7.11          7.11
    ## 2 1503960366 2016-03-26        17609          2154           11.6          11.6 
    ## 3 1503960366 2016-03-27        12736          1944            8.53          8.53
    ## 4 1503960366 2016-03-28        13231          1932            8.93          8.93
    ## 5 1503960366 2016-03-29        12041          1886            7.85          7.85
    ## 6 1503960366 2016-03-30        10970          1820            7.16          7.16
    ## # ℹ 1 more variable: DayOfWeek <chr>

### **Analyse and Visualize the data**

#### Looking for engagement in users

After we cleaning the data and put them into one dataset now we can
recall to our business task. To find the usage pattern in Bellabeat! Now
that we know out business task is, we can code this.

``` r
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
```

![](Project-1-Markdown_files/figure-gfm/Analyzing%20the%20data%201-1.png)<!-- -->

``` r
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
```

![](Project-1-Markdown_files/figure-gfm/Analyzing%20the%20data%201-2.png)<!-- -->

``` r
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
```

![](Project-1-Markdown_files/figure-gfm/Analyzing%20the%20data%201-3.png)<!-- -->

``` r
# see Saturday is the highest

# now that we know saturday has the most acitivy
# we are gonna count how many tracker distance that our user have only on saturday

trackerdistanceSATURDAYMEAN <- combined_activity_dataCLEAN %>% filter( DayOfWeek == "Saturday") %>% 
  summarize(totaltrackerdistance = mean(TrackerDistance))

# now that we know the average miles of trackerdistance in saturday we now compare it to the whole data average

TD_merged <- cbind(trackerDistanceMEAN,trackerdistanceSATURDAYMEAN)

# now we know that saturday really has slightly has higher usage, that means we are gonna leave it and draw a conclusion to make a decision
```

Now we know that we can create an engagement to the user on saturday,
which is the most active users are on that day.

#### Increasing Interest in Users

With that information we get, we now can create a code to know how many
calories are the users burn during the usage of our device, to nest it
on the notification we give on saturday with our marketing team.

``` r
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
```

![](Project-1-Markdown_files/figure-gfm/Data%20analyzing%202-1.png)<!-- -->

#### Individual Calculation

Wow that was the data of all users in the dataset and it is good for our
research and development team to know the data. We need to inform our
users about their achievement in their products. Let’s go

``` r
# we need to know how much each individual burn their calories so that we can
# encourage them to do more exercise!

user_avg_calories_per_day <- combined_activity_dataCALCLEAN %>%
  group_by(Id, ActivityDate) %>%
  summarize(AverageCaloriesPerDistance = mean(CaloriesPerDistance, 
                                              na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'Id'. You can override using the `.groups`
    ## argument.

``` r
# clean them one more time
user_avg_calories_per_dayCLEAN <- user_avg_calories_per_day %>%
  filter(!is.nan(AverageCaloriesPerDistance))

# with that much of users i will take 3 of them to visualize the average calories burns by the users

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
    axis.text.x = element_text(angle = 35, hjust = 1) 
  )
```

![](Project-1-Markdown_files/figure-gfm/Analyzing%20data%203-1.png)<!-- -->

``` r
# wow user 1624580081 really have exercise more 
```

### **Sharing Session**

Now that we know what we have been analyze I can now do a recomendation
for business question that was asked. \#### Users tends to use Bellabeat
product more in Saturday but not in Thursday and Wednesday

We can see that in this graphic

``` r
ggplot(data = combined_activity_dataCLEAN) + 
  geom_bar(mapping = aes(x = DayOfWeek, fill = DayOfWeek)) +
  geom_hline(yintercept = tuesday_count, color = "red", linetype = "dashed") +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  ) +
  labs(title = "Activity by Day of the Week", x = "Day of Week", y = "Count")
```

![](Project-1-Markdown_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

So with the graphic above we know the best day to remind, notified the
user is not in wednesday and Thursday, because it has the lowest
activity in a week.

#### Suggestion for Bellabeat in marketing side

As we already visualize the usage of each users, In that case we can
encourage users to exercise more. We have example in this graphic

``` r
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
    axis.text.x = element_text(angle = 35, hjust = 1) 
  )
```

![](Project-1-Markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### **Closing**

With the conclusion which I have already done, my Analysis in this first
case study of mine have finish.  
Thank you the readers have been reading this.
