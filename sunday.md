Analysis
================
Robert West
7/2/2020

# SUNDAY Report

## About the Data

The Dataset contains information about articles posted on
[Mashable](https://mashable.com/) that includes measures of sentiment
analysis. Being absolutely no expert in language analysis, I will need
to explore the data before modelling.

``` r
data_Total = read_csv("OnlineNewsPopularity.csv")
```

Upon review of the data, there are variables that I elected to not
consider for the analysis:

  - `url` (Described as non-predictive)  
  - `timedelta` (Described as non-predictive)  
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
  dplyr::select(-c(url, timedelta, LDA_00, LDA_01, LDA_02, LDA_03, LDA_04)) %>%
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

## Analysis Goals

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

#Filter by the day
data_Day = data_Analysis %>% 
  filter(data_Analysis[params$day] == 1) %>%
  mutate(shares_cat = as.factor(shares_cat))
```

## Train/Test split

Since there is a plethora of data for every day, I will do a 70:30 split
for training/testing respectively.

``` r
set.seed(623)
train = sample(1:nrow(data_Day), size = 0.7*nrow(data_Day))
test = dplyr::setdiff(1:nrow(data_Day), train)

data_Train = data_Day[train, ]
data_Test = data_Day[test, ]
```

## Data Summary

``` r
grouped_mean = data_Train %>%
  group_by(shares_cat) %>%
  summarise(
    n=n(),
    avg_Title_Char = round(mean(n_tokens_title), 3),
    avg_Content_Char = round(mean(n_tokens_content), 3),
    avg_hrefs = round(mean(num_hrefs), 3),
    avg_media = round(mean(num_imgs + num_videos), 3),
    avg_subjectivity = round(mean(global_subjectivity), 3), 
    avg_positive = round(mean(rate_positive_words), 3),
    avg_negative = round(mean(rate_negative_words), 3),
    avg_shares = round(mean(shares), 3)
  )
  
kable(grouped_mean, caption="Averages for each level of share volume")
```

| shares\_cat |    n | avg\_Title\_Char | avg\_Content\_Char | avg\_hrefs | avg\_media | avg\_subjectivity | avg\_positive | avg\_negative | avg\_shares |
| :---------- | ---: | ---------------: | -----------------: | ---------: | ---------: | ----------------: | ------------: | ------------: | ----------: |
| High        | 1322 |           10.407 |            627.113 |     13.603 |      7.207 |             0.456 |         0.693 |         0.280 |    5133.434 |
| Low         |  593 |           10.513 |            575.811 |     10.894 |      6.243 |             0.436 |         0.665 |         0.307 |    1044.976 |

Averages for each level of share volume

``` r
grouped_sd = data_Train %>%
  group_by(shares_cat) %>%
  summarise(
    n=n(),
    sd_Title_Char = round(sd(n_tokens_title), 3),
    sd_Content_Char = round(sd(n_tokens_content), 3),
    sd_hrefs = round(sd(num_hrefs), 3),
    sd_media = round(sd(num_imgs + num_videos), 3),
    sd_subjectivity = round(sd(global_subjectivity), 3), 
    sd_positive = round(sd(rate_positive_words), 3),
    sd_negative = round(sd(rate_negative_words), 3),
    sd_shares = round(sd(shares), 3)
  )

kable(grouped_sd, caption="Standard Deviations for each level of share volume")
```

| shares\_cat |    n | sd\_Title\_Char | sd\_Content\_Char | sd\_hrefs | sd\_media | sd\_subjectivity | sd\_positive | sd\_negative | sd\_shares |
| :---------- | ---: | --------------: | ----------------: | --------: | --------: | ---------------: | -----------: | -----------: | ---------: |
| High        | 1322 |           2.148 |           613.680 |    13.491 |    11.067 |            0.118 |        0.183 |        0.150 |   7785.770 |
| Low         |  593 |           2.000 |           454.583 |     9.531 |    10.278 |            0.118 |        0.196 |        0.168 |    220.104 |

Standard Deviations for each level of share volume

From the table above, it doesn’t seem like center and spread of the
levels of share volume differ too much in any variable that I
considered. This may make modeling and predicting a little harder
because the algorithm will have small differences to detect between the
two categories.

## Visual Exploration

I am unfamiliar with many variables in the dataset. Below are multiple
plots of variables such as the number of characters and tools for
sentiment analysis such as positive/negative word rate. In each plot, I
try to get a feel for the relationship of the share volume to each of
the variables.

``` r
ggplot(data=data_Train, aes(x=n_tokens_title, y=n_tokens_content))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Title Characters", y="Content Characters", color="Share Volume", title="Title Vs Content in Characters")
```

![](sunday_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=rate_positive_words, y=global_rate_positive_words))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Rate of Positive Words", y="Global Rate of Positive Words", color="Share Volume", title="Individual vs Global Positive Word Rate")
```

![](sunday_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=rate_negative_words, y=global_rate_negative_words))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Rate of Negative Words", y="Global Rate of Negative Words", color="Share Volume", title="Individual vs Global Negative Word Rate")
```

![](sunday_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=global_subjectivity, y=global_sentiment_polarity))+
  geom_jitter(aes(color = shares_cat))+
  labs(x="Global Subjectivity", y="Global Sentiment", title="Subjectivity Against Sentiment", color = "Share Volume")
```

![](sunday_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=shares_cat, y=n_tokens_title))+
  geom_boxplot(aes(color=shares_cat))+
  labs(y="Title Characters", x="Shares Volume", color="Shares Volume")
```

![](sunday_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=avg_positive_polarity, y=avg_negative_polarity))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Average Positive Polarity", y="Average Negative Polarity", color="Shares Volume", title="Average Positive vs Negative Polarity")
```

![](sunday_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

As a whole, these plots do not suggest that there is a huge difference
in behavior among high share volume and low share volume articles. This
reinforces my assumption that I may see some large misclassification
rates in my models.

## Models

To attempt to predict the relative popularity of a post, I will utilize
Logistic regression as a GLM and a random forest fit and tuned on a
training set and evaluated on a test set using misclassification rate as
the metric for selecting a better fitting model.

First, I need to grab the numeric variables from the training set:

``` r
train_Num = data_Train %>%
  dplyr::select(-c(is_weekend,
            monday,
            tuesday,
            wednesday,
            thursday,
            friday,
            saturday,
            sunday,
            shares))

test_Num = data_Test %>%
  dplyr::select(-c(is_weekend,
            monday,
            tuesday,
            wednesday,
            thursday,
            friday,
            saturday,
            sunday,
            shares))
```

## Fitting a Logistic Regression Model to the training set

Below, I fit the Logistic model using the `glm()` function and selected
a “best” model by forward select with AIC as the criteria. This is done
with AIC to attempt to allow the model to generalize better by finding a
balance between how well it explains the data and how complex it is.

``` r
library(MASS)

logistic_Fit = glm(shares_cat ~ ., data=train_Num, family="binomial") %>%
  stepAIC(direction = "forward", trace=FALSE, steps=100)
```

## Fitting a Random Forest to the training set

I utilized a Random Forest for my ensemble method. This was preferable
to me because I like that it chooses a subset of the predictors and
Bootstraps the data to get better prediction by aggregating across many
trees. The number of variables to use in the model is selected by 5 fold
repeated cross validation using the `caret` package with accuracy as the
metric.

``` r
trctrl = trainControl(method="repeatedcv", number=5, repeats = 3)

rf_Fit = train(shares_cat ~ ., data=train_Num, method="rf",
              trControl = trctrl,
              preProcess = c("center", "scale"),
              tuneLength = 10,
              ntree = 20)
rf_Fit
```

    ## Random Forest 
    ## 
    ## 1915 samples
    ##   45 predictor
    ##    2 classes: 'High', 'Low' 
    ## 
    ## Pre-processing: centered (45), scaled (45) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 1532, 1531, 1533, 1533, 1531, 1532, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.6946986  0.1385644
    ##    6    0.6985244  0.1769466
    ##   11    0.6960888  0.1755615
    ##   16    0.6859985  0.1506373
    ##   21    0.6948726  0.1748593
    ##   25    0.6931297  0.1700542
    ##   30    0.6938196  0.1734203
    ##   35    0.6892825  0.1633311
    ##   40    0.6871997  0.1640020
    ##   45    0.6872051  0.1617975
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 6.

``` r
ggplot(data=rf_Fit)
```

![](sunday_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Fitting a K-Nearest-Neighbors model to the training set

Out of curiosity, since our data is primarily numeric, I wanted to fit a
KNN model as well to see how well it stacks against my linear model and
ensemble model. This is all done using the `caret` package. The best `k`
to use was selected by using repeated 5 fold cross validation with
accuracy as the metric.

``` r
trctrl = trainControl(method="repeatedcv", number=5, repeats=3)

knn_Fit = train(shares_cat ~., data=train_Num, method="knn",
                trControl = trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)
knn_Fit
```

    ## k-Nearest Neighbors 
    ## 
    ## 1915 samples
    ##   45 predictor
    ##    2 classes: 'High', 'Low' 
    ## 
    ## Pre-processing: centered (45), scaled (45) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 1532, 1532, 1532, 1532, 1532, 1532, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   Accuracy   Kappa    
    ##    5  0.6663357  0.1791376
    ##    7  0.6792105  0.1903674
    ##    9  0.6736500  0.1689951
    ##   11  0.6795673  0.1779380
    ##   13  0.6865213  0.1874852
    ##   15  0.6887886  0.1882396
    ##   17  0.6936661  0.1937185
    ##   19  0.6962712  0.1965153
    ##   21  0.6978382  0.1969698
    ##   23  0.6990517  0.1947874
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 23.

``` r
ggplot(data=knn_Fit)
```

![](sunday_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Comparing Test Misclassification Rate

The metric that I use to compare models is their misclassification rate
on the test set. I could normally not get a training set below 20%
misclassification, so I imagine that these will be a good bit higher on
data that the model has not seen
before.

``` r
logistic_Pred = predict(logistic_Fit, type = "response", newdata = test_Num)
logistic_Pred_Class = ifelse(logistic_Pred > 0.5, "High", "Low")
logistic_Misclass = mean(logistic_Pred_Class != data_Test$shares_cat)

rf_Pred = predict(rf_Fit, newdata = test_Num)
rf_Misclass = mean(rf_Pred != test_Num$shares_cat)

knn_Pred = predict(knn_Fit, newdata = test_Num)
knn_Misclass = mean(knn_Pred != test_Num$shares_cat)

kable(data.frame(Logistic = logistic_Misclass, RF = rf_Misclass, KNN = knn_Misclass), caption = "MisClassification Rate")
```

|  Logistic |        RF |       KNN |
| --------: | --------: | --------: |
| 0.7055961 | 0.3272506 | 0.3187348 |

MisClassification Rate

## Conclusion

For this day, of the models that we fit, the best test misclassification
rate was 31.9%. Logistic Regression tended to perform very weakly to the
other model. With a misclassification rate of 70.5596107%, we would be
better off always predicting one class since the counts of high and low
volume of shares is relatively similar. KNN usually tended to be a
better classifier, but since it was out of bounds for the project, the
clear winner for predicting share volume is the Random Forest model.
