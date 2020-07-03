Analysis
================
Robert West
7/2/2020

# SATURDAY Report

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

## Data Exploration

``` r
ggplot(data=data_Train, aes(x=n_tokens_title, y=n_tokens_content))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Title Characters", y="Content Characters", color="Share Volume", title="Title Vs Content in Characters")
```

![](saturday_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=rate_positive_words, y=global_rate_positive_words))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Rate of Positive Words", y="Global Rate of Positive Words", color="Share Volume", title="Individual vs Global Positive Word Rate")
```

![](saturday_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=rate_negative_words, y=global_rate_negative_words))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Rate of Negative Words", y="Global Rate of Negative Words", color="Share Volume", title="Individual vs Global Negative Word Rate")
```

![](saturday_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=global_subjectivity, y=global_sentiment_polarity))+
  geom_jitter(aes(color = shares_cat))+
  labs(x="Global Subjectivity", y="Global Sentiment", title="Subjectivity Against Sentiment", color = "Share Volume")
```

![](saturday_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=shares_cat, y=n_tokens_title))+
  geom_boxplot(aes(color=shares_cat))+
  labs(y="Title Characters", x="Shares Volume", color="Shares Volume")
```

![](saturday_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

``` r
ggplot(data=data_Train, aes(x=avg_positive_polarity, y=avg_negative_polarity))+
  geom_jitter(aes(color=shares_cat))+
  labs(x="Average Positive Polarity", y="Average Negative Polarity", color="Shares Volume", title="Average Positive vs Negative Polarity")
```

![](saturday_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

To attempt to predict the relative popularity of a post, I will utilize
Logistic regression as a GLM and a random forest fit and tuned on a
training set and evaluated on a test set using misclassification rate as
the metric for selecting a better fitting model.

First, I need to grab the numeric variables from the training set

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
repeated cross validation using the `caret` package.

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
    ## 1717 samples
    ##   45 predictor
    ##    2 classes: 'High', 'Low' 
    ## 
    ## Pre-processing: centered (45), scaled (45) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 1373, 1375, 1373, 1374, 1373, 1373, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.7565660  0.1942128
    ##    6    0.7610234  0.2333650
    ##   11    0.7569502  0.2245115
    ##   16    0.7542144  0.2250933
    ##   21    0.7557716  0.2270946
    ##   25    0.7489784  0.2204376
    ##   30    0.7587006  0.2437250
    ##   35    0.7540359  0.2394396
    ##   40    0.7542393  0.2392953
    ##   45    0.7540320  0.2528100
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 6.

``` r
ggplot(data=rf_Fit)
```

![](saturday_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## K-Nearest-Neighbors

Out of curiosity, since our data is primarily numeric, I wanted to fit a
KNN model as well to see how well it stacks against our linear model and
ensemble model. This is all done using the `caret` package.

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
    ## 1717 samples
    ##   45 predictor
    ##    2 classes: 'High', 'Low' 
    ## 
    ## Pre-processing: centered (45), scaled (45) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 1374, 1374, 1373, 1373, 1374, 1373, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   Accuracy   Kappa    
    ##    5  0.7160056  0.2063143
    ##    7  0.7214274  0.2121635
    ##    9  0.7272487  0.2199677
    ##   11  0.7328785  0.2289616
    ##   13  0.7344345  0.2311472
    ##   15  0.7321039  0.2192369
    ##   17  0.7317179  0.2101773
    ##   19  0.7379291  0.2203263
    ##   21  0.7354046  0.2095228
    ##   23  0.7420057  0.2254358
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 23.

``` r
ggplot(data=knn_Fit)
```

![](saturday_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Comparing Out-Of-Sample Misclassification Rate

``` r
logistic_Pred = predict(logistic_Fit, type = "response", newdata = test_Num)
logistic_Pred_Class = ifelse(logistic_Pred > 0.5, "High", "Low")
logistic_MisClass = mean(logistic_Pred_Class != data_Test$shares_cat)

rf_Pred = predict(rf_Fit, newdata = test_Num)
rf_MisClass = mean(rf_Pred != test_Num$shares_cat)

knn_Pred = predict(knn_Fit, newdata = test_Num)
knn_MisClass = mean(knn_Pred != test_Num$shares_cat)

kable(data.frame(Logistic = logistic_MisClass, RF = rf_MisClass, KNN = knn_MisClass), caption = "MisClassification Rate")
```

| Logistic |        RF |       KNN |
| -------: | --------: | --------: |
| 0.763587 | 0.2391304 | 0.2554348 |

MisClassification Rate