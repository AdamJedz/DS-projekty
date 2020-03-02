xgb wo splitting
================
Bartosz Adamiec
17 02 2020

``` r
# xgboost

#environment preparation and data load
my_theme <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text())

mydata_train <- read.csv("./train_set.csv", stringsAsFactors = FALSE, na.strings="")

mydata_test <- read.csv("./test_set.csv", stringsAsFactors = FALSE, na.strings="")

xgg_train <- mydata_train %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms)

xgg_test <- mydata_test %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms)



xgg_train <- xgg_train %>% mutate_if(is.character,as.factor)
xgg_test <- xgg_test %>% mutate_if(is.character,as.factor)


xgg_train$duration_ms <- scale(xgg_train$duration_ms)
xgg_test$duration_ms <- scale(xgg_test$duration_ms)



str(xgg_train)
```

    ## 'data.frame':    26266 obs. of  14 variables:
    ##  $ playlist_genre  : Factor w/ 6 levels "edm","latin",..: 2 3 1 3 3 5 2 5 4 2 ...
    ##  $ track_popularity: int  65 0 31 81 73 83 73 59 47 51 ...
    ##  $ danceability    : num  0.816 0.611 0.641 0.649 0.774 0.687 0.635 0.79 0.594 0.895 ...
    ##  $ energy          : num  0.754 0.988 0.874 0.716 0.75 0.449 0.713 0.799 0.521 0.43 ...
    ##  $ key             : int  11 10 1 8 11 2 11 6 0 3 ...
    ##  $ loudness        : num  -2.75 -3.97 -4.99 -5.37 -4.93 ...
    ##  $ mode            : int  0 1 1 1 0 1 1 1 1 0 ...
    ##  $ speechiness     : num  0.111 0.102 0.0746 0.0349 0.0413 0.154 0.0937 0.15 0.0417 0.143 ...
    ##  $ acousticness    : num  0.0361 0.00254 0.136 0.0863 0.0021 0.109 0.228 0.123 0.161 0.126 ...
    ##  $ instrumentalness: num  0.00 7.84e-01 2.44e-06 2.63e-05 1.85e-05 0.00 0.00 0.00 5.56e-03 0.00 ...
    ##  $ liveness        : num  0.217 0.222 0.199 0.135 0.0808 0.282 0.0506 0.0575 0.0382 0.0701 ...
    ##  $ valence         : num  0.533 0.269 0.14 0.163 0.925 0.229 0.749 0.794 0.789 0.793 ...
    ##  $ tempo           : num  100 136 128 100 118 ...
    ##  $ duration_ms     : num [1:26266, 1] -1.126 2.542 -0.812 -0.332 -0.465 ...
    ##   ..- attr(*, "scaled:center")= num 225836
    ##   ..- attr(*, "scaled:scale")= num 59897

``` r
genres = xgg_train$playlist_genre
label = as.integer(xgg_train$playlist_genre)-1
label_test = as.integer(xgg_test$playlist_genre)-1
xgg_train$playlist_genre = NULL
xgg_test$playlist_genre = NULL
```

``` r
train.data = as.matrix(xgg_train)
train.label = label
test.data = as.matrix(xgg_test)
test.label = label_test

nrow(train.data)
```

    ## [1] 26266

``` r
length(train.label)
```

    ## [1] 26266

``` r
length(test.label)
```

    ## [1] 6567

``` r
nrow(test.data)
```

    ## [1] 6567

``` r
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)
```

``` r
num_class = length(levels(genres))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=10,
  gamma=5,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)
```

``` r
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=2000,
  # nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit
```

    ## ##### xgb.Booster
    ## raw: 336.6 Mb 
    ## call:
    ##   xgb.train(params = params, data = xgb.train, nrounds = 2000, 
    ##     watchlist = list(val1 = xgb.train, val2 = xgb.test), verbose = 0, 
    ##     early_stopping_rounds = 10)
    ## params (as set within xgb.train):
    ##   booster = "gbtree", eta = "0.001", max_depth = "10", gamma = "5", subsample = "0.75", colsample_bytree = "1", objective = "multi:softprob", eval_metric = "mlogloss", num_class = "6", silent = "1"
    ## xgb.attributes:
    ##   best_iteration, best_msg, best_ntreelimit, best_score, niter
    ## callbacks:
    ##   cb.evaluation.log()
    ##   cb.early.stop(stopping_rounds = early_stopping_rounds, maximize = maximize, 
    ##     verbose = verbose)
    ## # of features: 13 
    ## niter: 2000
    ## best_iteration : 2000 
    ## best_ntreelimit : 2000 
    ## best_score : 1.263173 
    ## nfeatures : 13 
    ## evaluation_log:
    ##     iter val1_mlogloss val2_mlogloss
    ##        1      1.790830      1.790987
    ##        2      1.789900      1.790218
    ## ---                                 
    ##     1999      1.128073      1.263258
    ##     2000      1.127958      1.263173

``` r
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(genres)
```

``` r
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(genres)[test.label+1]
```

``` r
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))
```

    ## [1] "Final Accuracy = 55.98%"
