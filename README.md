README
================
Robert West
6/30/2020

# Reading in the Data

``` r
data_Total = read_csv("OnlineNewsPopularity.csv")
```

Upon review of the data, there are variables that I elected to not
consider for the analysis: url timedelta

``` r
data_Analysis = data_Total %>%
  select(-c(url, timedelta)) %>%
  mutate(
    monday = weekday_is_monday,
    tuesday = weekday_is_tuesday,
    wednesday = weekday_is_wednesday,
    thursday = weekday_is_thursday,
    friday = weekday_is_friday,
    saturday = weekday_is_saturday,
    sunday = weekday_is_sunday,
    .keep = "unused"
  )
```

# Data

# Analysis Goals

Overall, the goal of this analysis is to predict the `shares` variable
for each post. While this may be important, I find it more interesting
to attempt to classify a given post as high volume (More than 1400
shares) or low volume (At most 1400 shares) based on the variables that
we are given. I think this will give us higher predictive power because
models that are built solely on numeric data tend to give larger errors
in extreme cases of only a handful of shares and thousands of shares.
Practically, we care less about the actual number of shares that a post
receives and more about its magnitude. This analysis will be done for
every day of the
week

``` r
data_Analysis$shares_cat = ifelse(data_Analysis$shares >= 1400, "High", "Low")

data_day = data_Analysis %>% filter(monday == 1)
```

# Train/Test split

Since there is a plethora of data for every day, I will do a 70:30 split
for training/testing respectively

``` r
train = sample(1:nrow(data_day), size = 0.7*nrow(data_day))
test = dplyr::setdiff(1:nrow(data_day), train)

data_train = data_day[train, ]
data_test = data_day[test, ]
```
