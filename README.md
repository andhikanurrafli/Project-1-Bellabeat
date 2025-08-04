Bellabeat User Behavior Data Analysis
================
Andhika Nurrafli Putra
2025-06-03

## Background

This a fictional case in Bellabeat where I the stakeholders told me to
analyse the behavior of users in Using the bellabeat. In this case I
will use RStudio for analyzing it.

## Determining the Business question

To make this project clear I’m aiming some specific targets, which is:  
1. What is the trends in using the smart gadget.  
2. How this trends can be apply in Bellabeat’s Customers.  
3. How is this trends can help influence the Bellabeats marketing
strategy.  

Base on this targets the output I hope is to provide some recommendation
about the marketing strategies that can help boost Bellabeat usage.  

> **Goal:** Understand user behavior and usage patterns to develop
> marketing strategies and recommendations for Bellabeat.

### Loading the Environment for analysis

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(knitr)
library(lubridate)
```

## Preparing the Data

### Dataset

The dataset I use is from Mobius in
[Kaggle](https://www.kaggle.com/arashnic).

### Loading the Data

``` r
dailyActivity <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
heartrate_seconds <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")
hourlyCalories <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
hourlyIntensities <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv")
hourlySteps <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv")
minutesSleeps <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")
WeightLog <- read.csv("D:/Studying/Data Analyst Projects/Project 1/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")
```

## Processing the Data

### Checking data for error

Changing data type for date into

``` r
minutesSleeps <- minutesSleeps %>% 
  mutate(date = mdy_hms(date))

summarySleep <- minutesSleeps %>% 
  group_by(Id) %>%
  summarise(total_sleep = as.numeric(difftime(max(date), min(date), units = "mins")))

dailyActivity <- dailyActivity %>% 
  mutate(ActivityDate = mdy(ActivityDate))

hourlyCalories <- hourlyCalories %>% 
  mutate(ActivityHour = mdy_hms(ActivityHour))
```

### Cleaning the Data

Checking the data if there’s not valid data I will remove rows in it.

``` r
# Cleaning if there is an empty cell in the data frame
minutesSleeps <- drop_na(minutesSleeps)

summarySleep <-  drop_na(summarySleep)

dailyActivity <-  drop_na(dailyActivity)

hourlyCalories <- drop_na(hourlyCalories)
```

### Exploring the Data

Exploring the data for analysis base on the business questions. So first
I will look at the correlation between total distance vs calories.

``` r
dailyActivity %>% select(TotalSteps,TotalDistance, Calories) %>% 
  summary()
```

    ##    TotalSteps    TotalDistance       Calories   
    ##  Min.   :    0   Min.   : 0.000   Min.   :   0  
    ##  1st Qu.: 1988   1st Qu.: 1.410   1st Qu.:1776  
    ##  Median : 5986   Median : 4.090   Median :2062  
    ##  Mean   : 6547   Mean   : 4.664   Mean   :2189  
    ##  3rd Qu.:10198   3rd Qu.: 7.160   3rd Qu.:2667  
    ##  Max.   :28497   Max.   :27.530   Max.   :4562

``` r
cor(dailyActivity$Calories, dailyActivity$TotalSteps)
```

    ## [1] 0.5813802

The correlation obtained is 0.5813802, which indicates a moderate to
strong positive relationship between the two variables. This suggests
that as the total distance increases, the number of calories burned also
tends to increase accordingly.  
  
Now we visualize it

``` r
correlation_total_step_vs_calories <- ggplot(dailyActivity, aes(x = Calories, y = TotalSteps))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = "lm", color = "red")+
  geom_hline(yintercept = mean(dailyActivity$TotalSteps),
             linetype = "dashed", color = "blue")+
  geom_vline(xintercept = mean(dailyActivity$Calories),
             linetype = "dashed", color = "green")+
  labs(
    title = "Correlation Total Steps vs Calories\nwith Mean"
  )

correlation_total_step_vs_calories
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Now that we got our firsts glimpses of the data we can see that we
actually have strong positive relationship between Total Steps vs
calories. That means the more our user walk or run the more calories
that they burn.  
We can see at the visualization there is still a lot of users that have
below average in calories burnt, total steps, and total distance. In
that case we can take that as a trend that we will evaluate and analyze.
But firstly we need to find out the average and total of intensity that
the user made per date.

``` r
hourlyIntensities$ActivityDate <- as.Date(mdy_hms(hourlyIntensities$ActivityHour))


mean_intensity_per_day <- hourlyIntensities %>% 
  group_by(ActivityDate) %>% 
  summarise(MeanIntensity = mean(TotalIntensity, na.rm = TRUE))

ggplot(mean_intensity_per_day, aes(x=ActivityDate,y=MeanIntensity )) +
  geom_line(color = "darkgreen") +
  geom_point(color = "black", size = 1.5)+
  labs (
    title = "Average Daily Intensity Over Time",
    x = "date",
    y = "average intensity"
  )+
  theme_minimal()
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
total_intensity_per_day_per_id <- hourlyIntensities %>% 
  group_by(Id, ActivityDate) %>% 
  summarise(TotalIntensity = sum(TotalIntensity, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'Id'. You can override using the `.groups`
    ## argument.

``` r
ggplot(total_intensity_per_day_per_id, aes(x=ActivityDate,y=TotalIntensity)) +
  geom_col(fill= "darkcyan") +
  labs(title = "Total Intensity per day",
       x = "Date",
       y = "Intensity")+
  theme_minimal()
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

So we have see the average and total of the intensity later we will
analyze it and find out how the trends is.  

### Analyzing the Data

We are on the analyze phase now, that mean we will processing the data
that we need and do some analysis about the data that we have define.
**First**, We will see how the distribution of the data we need to
filter out the outliers

``` r
total_intensity_per_day_per_id <- total_intensity_per_day_per_id %>% 
  mutate(
    Day_of_week = wday(ActivityDate,week_start = 7, label = TRUE)
  )



#doing boxplot

distribution_of_total_intensity <- ggplot(total_intensity_per_day_per_id, aes(x=Day_of_week, y = TotalIntensity)) +
  geom_boxplot()+
  labs(
    title = "Distribution of Total intensity by\nDay of Week",
    x = "Day of Week",
    y= "Total Intensity"
  ) +
  theme_minimal()

distribution_of_total_intensity
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

So we got small outliers there, we need to define each quartil of the
data and filter out Q0 to Q1.

``` r
# Determining the outlier in our data

Q1 <- quantile(total_intensity_per_day_per_id$TotalIntensity, 0.25, na.rm = TRUE)
Q3 <- quantile(total_intensity_per_day_per_id$TotalIntensity, 0.75, na.rm = TRUE)
IQR_Value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_Value
upper_bound <- Q3 + 1.5 * IQR_Value

total_intensity_per_day_per_id_no_outlier <- total_intensity_per_day_per_id %>%
  filter(TotalIntensity >= lower_bound & TotalIntensity <= upper_bound)

# vizkan

distribution_of_total_intensity_no_outlier <- ggplot(total_intensity_per_day_per_id_no_outlier, aes( y = TotalIntensity)) +
  geom_boxplot()+
  labs(
    title = "Distribution of Total intensity by\nDay of Week",
    x = "Day of Week",
    y= "Total Intensity"
  ) +
  theme_minimal()

distribution_of_total_intensity_no_outlier
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Finally we filtered out the outlier in our data, so on the next step we
will try to find out why the users have low activity.  
  
**Second**, we need to find out why they have low activities, we will
try analyze them base on the day or time of the activities.

``` r
# First let see how the data looks like after we filtered out the outliers
total_intensity_per_day_of_week <- ggplot(total_intensity_per_day_per_id_no_outlier, aes(x = Day_of_week, y = TotalIntensity))+
         geom_col(fill="darkcyan") +
  labs(
    title = "Total Intensity per Day of Week",
    x = "Day",
    y = "Intensity"
  ) +
  theme_minimal()
total_intensity_per_day_of_week
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

We find that Tuesday to Friday Users have the lowest intensity, it is
more likely they are working in office that have less intensity, on
Monday it is also a workday but Monday usually has a lot of activity in
the office, so people are more hectic than usual work days.  
To prove this we need to find out the amount of steps each day every
day.

``` r
hourlySteps$Activityday <- as.Date(mdy_hms(hourlySteps$ActivityHour))

mean_steps_per_day <- hourlySteps %>% 
  group_by(Activityday) %>% 
  summarise(MeanSteps = mean(StepTotal, na.rm = TRUE))

mean_steps_per_day <- mean_steps_per_day %>% 
  mutate(
    Day_of_Week = wday(Activityday, week_start = 7, label = TRUE)
  )

average_steps_per_day_of_week <- ggplot(mean_steps_per_day, aes(x = Day_of_Week, y = MeanSteps ))+
  geom_col(fill = "darkcyan") + 
  labs(
    title = "Average Steps Per Day of Week",
    x = "Day",
    y = "Steps"
  )+
  theme_minimal()

average_steps_per_day_of_week
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

and yes we see the average steps of users from Monday to Friday are
declining, that is as I said before it is more likely that they got
hectic on monday and more stable on the next week days. For the weekends
we can see that they has a lot of steps on Saturday and less on Sunday,
it is more likely because they wanted to take a rest on Sunday and doing
exercise, shopping or doing recreation stuff on Saturday.  

For more clear view of the data we are going to visualize it on daily
basis and hourly basis.

``` r
# Hour by Hour


hourlySteps <- hourlySteps %>% 
  mutate(
    ActivityHour_parsed = mdy_hms(ActivityHour),
    time = format(ActivityHour_parsed, "%H:%M:%S")
  )

sum_hourlysteps <- hourlySteps %>% 
  group_by(Activityday,time) %>% 
  summarise(totalsteps = sum(StepTotal, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'Activityday'. You can override using the
    ## `.groups` argument.

``` r
sum_time_hourlysteps <- sum_hourlysteps %>% 
  group_by(time) %>% 
  summarise(totalsteps = sum(totalsteps, na.rm = TRUE))

total_steps_per_hour_in_days <- ggplot(sum_hourlysteps, aes(x = time, y = totalsteps)) +
  geom_col(fill = "darkcyan") +
  labs(
    title = "Total Steps per Hour in Days",
    x = "Time",
    y = "Total Steps"
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust= 1))

total_steps_per_hour_in_days
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Understanding how active users are throughout the day provides a
foundation for analyzing whether their activity influences the quality
and duration of their sleep.  

**Third**, Analyzing sleep.  

At this stage, the value column contains three categories: 1 (Sleep), 2
(Awake), and 3 (Restless). Based on this classification, I will proceed
with the analysis.

``` r
# First we will look how the data.

minutesSleeps_cleaned <- minutesSleeps %>% 
  mutate(date_only = as.Date(date),
         hour = hour(date)) %>% 
  group_by(Id, date_only, hour) %>% 
  summarise(avg_value = mean(value), .groups = "drop")

minutessleeps_weekly <- minutesSleeps %>% 
  mutate(weekday = wday(date, label = TRUE),
         hour = hour(date)) %>% 
  group_by(Id, weekday, hour) %>% 
  summarise(avg_state = mean(value), .groups = "drop")



# we need samples for our data so that we can easily visualize it.

set.seed(123)
sample_ids <- sample(unique(minutesSleeps_cleaned$Id), size = 8)


minutesSleeps_sample <- minutessleeps_weekly %>% 
  filter(Id %in% sample_ids)

ggplot(minutesSleeps_sample, aes(x = hour, y = fct_rev(weekday), fill = avg_state)) +
  geom_tile(color = "white") +
  facet_wrap(~ Id, scales = "free_y")+
  scale_fill_gradientn(colors = c("green", "orange", "red"),
                       name = "Sleep State",
                       breaks = c(1, 2, 3),
                       labels = c("Sleep", "Awake", "Restless")) +
  labs(
    title = "Weekly Sleep Pattern (Hourly Average)",
    x = "Hour of Day",
    y = "Day of Week") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14))
```

![](Project-Bellabeat-Markdown_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Based on the visualization above, it can be observed that among the
randomly selected sample, some individuals consistently use the
Bellabeat application, while others have incomplete data records.
However, from the complete data available, we can see that users tend to
sleep in accordance with their regular sleep schedules.

## Sharing Findings

From the analysis conducted, it can be concluded that the available
dataset may not be sufficient for drawing comprehensive insights, as a
larger amount of data is needed for a more robust analysis. However,
based on the data that is available, we were able to identify several
key findings:

- **First**, there is a clear correlation between total steps and
  calories burned. This suggests that individuals who take more steps
  tend to burn more calories.
- **Second**, it was observed that users tend to take more steps on
  Mondays and Saturdays. This may indicate that these days involve
  activities that require more physical movement, such as commuting at
  the start of the week or engaging in leisure activities over the
  weekend.
- **Third**, while many users show a relatively normal sleep pattern,
  some data appears to be incomplete or inconsistent, making it
  difficult to fully analyze sleeping habits.

Further data collection and refinement are recommended for deeper and
more reliable insights.

### Recommendations

Based on the findings and limitations of the current analysis, the
following recommendations are proposed:

- **Expand Data Collection**: To draw more accurate conclusions, it is
  essential to collect a larger and more diverse dataset. This includes
  ensuring consistent tracking across all users to avoid missing or
  incomplete data, especially in sleep tracking.

- **Encourage Regular Tracking**: Users should be encouraged to
  consistently wear and sync their Bellabeat devices, particularly
  during sleep hours, to improve the reliability of sleep data and allow
  for more meaningful insights into their health patterns.

- **Promote Active Days**: Since high step counts are observed on
  Mondays and Saturdays, Bellabeat can design targeted wellness
  challenges or reminders on these days to further engage users and
  promote regular physical activity.

- **Enhance Insights through Personalized Feedback**: Incorporate
  personalized feedback in the Bellabeat app based on a user’s activity
  and sleep patterns. This can increase user motivation and help them
  better understand how their behavior impacts their overall wellness.

- **Future Studies**: Future analyses should consider seasonal,
  demographic, or lifestyle factors to identify deeper trends and tailor
  recommendations according to user segments.
