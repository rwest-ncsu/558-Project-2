README
================
Robert West
6/30/2020

# Reading in the Data

``` r
data_Total = read_csv("OnlineNewsPopularity.csv")
```

# Data

The Dataset above contains information about articles posted on
[Mashable](https://mashable.com/) that includes measures of sentiment
analysis. Being absolutely no expert in language analysis, I will need
to explore the data before modelling.

Upon review of the data, there are variables that I elected to not
consider for the analysis:

  - `url`  
  - `timedelta`  
  - `Closeness to LDA 0`
  - `Closeness to LDA 1`  
  - `Closeness to LDA 2`  
  - `Closeness to LDA 3`  
  - `Closeness to LDA 4`

URL and timedelta are listed as non-predicting in the dataset, and the
LDA variables are unclear in nature, but I believe they are related to a
natural language processing tool called [Latent Dirichlet
allocation](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation).
Without knowledge of this type of analysis, I felt uncomfortable
including these variables in my own.

``` r
data_Analysis = data_Total %>%
  select(-c(url, timedelta, LDA_00, LDA_01, LDA_02, LDA_03, LDA_04)) %>%
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

# Analysis Goals

Overall, the goal of this analysis is to predict the `shares` variable
for each article. While this may be important, I find it more
interesting to attempt to classify a given article as high volume (More
than 1400 shares) or low volume (At most 1400 shares) based on the
variables that we are given. I think this will give us higher predictive
power because models that are built solely on numeric data tend to give
larger errors in extreme cases of only a handful of shares and thousands
of shares. Additionally, we practically care less about the actual
number of shares that a post receives and more about its magnitude. This
analysis will be done for every day of the
week.

``` r
data_Analysis$shares_cat = ifelse(data_Analysis$shares >= 1400, "High", "Low")

data_day = data_Analysis %>% 
  filter(monday == 1) 
```

# Data Exploration

``` r
ggplot(data=data_day, aes(x=n_tokens_title, y=n_tokens_content))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Title Characters", y="Content Characters", color="Share Volume", title="Title Vs Content in Characters")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(data=data_day, aes(x=rate_positive_words, y=global_rate_positive_words))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Rate of Positive Words", y="Global Rate of Positive Words", color="Share Volume", title="Individual vs Global Positive Word Rate")
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
ggplot(data=data_day, aes(x=rate_negative_words, y=global_rate_negative_words))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Rate of Negative Words", y="Global Rate of Negative Words", color="Share Volume", title="Individual vs Global Negative Word Rate")
```

![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
ggplot(data=data_day, aes(x=global_subjectivity, y=global_sentiment_polarity))+
  geom_jitter(aes(color = shares_cat))+
  labs(x="Global Subjectivity", y="Global Sentiment", title="Subjectivity Against Sentiment", color = "Share Volume")
```

![](README_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
ggplot(data=data_day, aes(x=shares_cat, y=n_tokens_title))+
  geom_boxplot(aes(color=shares_cat))+
  labs(y="Title Characters", x="Shares Volume", color="Shares Volume")
```

![](README_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
ggplot(data=data_day, aes(x=avg_positive_polarity, y=avg_negative_polarity))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Average Positive Polarity", y="Average Negative Polarity", color="Shares Volume", title="Average Positive vs Negative Polarity")
```

![](README_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

To attempt to predict the relative popularity of a post, I will utilize
Logistic regression as a GLM and a random forest fit and tuned on a
training set and evaluated on a test set using misclassification rate as
the metric for selecting a better fitting model.

# Train/Test split

Since there is a plethora of data for every day, I will do a 70:30 split
for training/testing respectively

``` r
train = sample(1:nrow(data_day), size = 0.7*nrow(data_day))
test = dplyr::setdiff(1:nrow(data_day), train)

data_train = data_day[train, ]
data_test = data_day[test, ]
```

# Fitting a Logistic Regression Model to the training set
