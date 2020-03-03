naivebayes
================
Bartosz Adamiec
03 03 2020

``` r
# Random Forest 

#environment preparation and data load
my_theme <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text())

mydata_train <- read.csv("./train_set.csv", stringsAsFactors = FALSE, na.strings="")

mydata_test <- read.csv("./test_set.csv", stringsAsFactors = FALSE, na.strings="")

nb_train <- mydata_train %>% dplyr::select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms)

nb_test <- mydata_test %>% dplyr::select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms)



nb_train <- nb_train %>% mutate_if(is.character,as.factor)
nb_test <- nb_test %>% mutate_if(is.character,as.factor)


nb_train$duration_ms <- scale(nb_train$duration_ms)
nb_test$duration_ms <- scale(nb_test$duration_ms)

str(nb_train)
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

# `{r} # nb_train$key <- train_key # nb_test$key <- test_key # nb_train$mode <- train_mode # nb_test$mode <- test_mode #`

``` r
train_key <- nb_train$key
test_key <- nb_test$key
train_mode <- nb_train$mode
test_mode <- nb_train$mode

nb_train$key <- NULL
nb_test$key <- NULL
nb_train$mode <- NULL
nb_test$mode <- NULL
```

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 881

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1411

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1431

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1695

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2473

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 9

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 28

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 39

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 64

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 71

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 114

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 133

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 138

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 142

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 158

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 183

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 256

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 275

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 402

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 404

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 471

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 516

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 544

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 561

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 565

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 583

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 631

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 698

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 735

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 745

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 767

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 817

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 855

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 875

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 881

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 918

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 945

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 955

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 962

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 966

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 975

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1004

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1124

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1144

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1167

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1170

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1191

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1279

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1290

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1311

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1315

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1358

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1431

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1440

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1447

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1471

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1484

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1504

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1540

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1552

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1556

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1605

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1650

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1689

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1695

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1698

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1718

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1728

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1774

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1776

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1787

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1815

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1846

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1871

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1928

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2016

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2048

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2050

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2114

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2198

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2217

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2223

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2294

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2297

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2301

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2314

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2345

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2356

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2473

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2498

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2526

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2533

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2535

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2547

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2578

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 340

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 800

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 897

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1062

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1097

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1266

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1715

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1825

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1831

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1876

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1887

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2040

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2110

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2286

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 47

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 63

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 69

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 70

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 122

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 123

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 181

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 182

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 200

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 222

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 262

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 275

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 284

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 340

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 385

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 421

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 456

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 507

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 538

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 599

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 617

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 629

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 642

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 673

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 687

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 700

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 710

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 743

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 754

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 800

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 817

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 818

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 829

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 839

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 867

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 870

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 897

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 902

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 941

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 974

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 976

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1006

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1009

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1022

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1062

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1065

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1097

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1099

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1117

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1140

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1157

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1188

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1208

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1254

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1266

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1295

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1344

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1352

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1355

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1368

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1423

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1433

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1448

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1469

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1493

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1522

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1614

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1648

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1700

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1714

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1715

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1765

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1767

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1772

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1781

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1783

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1816

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1831

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1876

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1887

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1905

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1906

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1914

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1936

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2021

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2040

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2072

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2110

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2125

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2131

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2151

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2173

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2192

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2223

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2286

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2306

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2346

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2391

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2395

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2413

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2432

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2485

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2497

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2557

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2586

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2591

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2598

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 166

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 361

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 416

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 432

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 523

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 550

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 696

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 840

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1381

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1426

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1534

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1899

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2153

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2422

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 124

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 146

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 166

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 177

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 195

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 202

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 209

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 245

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 350

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 351

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 369

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 388

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 416

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 432

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 433

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 463

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 466

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 474

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 495

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 533

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 550

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 559

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 574

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 585

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 586

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 615

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 653

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 696

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 719

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 739

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 755

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 800

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 819

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 840

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 849

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 853

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 864

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 892

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 893

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 911

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 937

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 947

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 948

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 970

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 979

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 985

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1021

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1042

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1057

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1088

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1097

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1105

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1164

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1223

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1224

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1229

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1279

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1326

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1337

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1381

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1399

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1418

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1482

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1502

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1519

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1534

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1573

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1626

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1693

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1756

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1801

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1803

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1855

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1879

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1889

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1898

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1899

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1961

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1967

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2020

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2072

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2153

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2180

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2222

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2228

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2278

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2303

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2316

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2324

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2403

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2422

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2485

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2531

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2547

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2610

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2623

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 7

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 541

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 956

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1214

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1536

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1956

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2149

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2303

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2426

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 7

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 19

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 51

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 66

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 81

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 99

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 123

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 162

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 174

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 220

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 234

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 238

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 251

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 290

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 323

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 335

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 379

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 439

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 444

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 457

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 475

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 494

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 541

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 547

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 549

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 556

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 602

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 614

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 630

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 652

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 687

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 689

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 716

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 717

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 750

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 762

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 763

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 782

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 785

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 794

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 869

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 886

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 894

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 936

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 937

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 956

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 970

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 987

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1001

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1032

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1037

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1041

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1051

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1064

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1096

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1193

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1194

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1199

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1219

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1239

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1264

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1292

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1297

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1300

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1319

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1326

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1369

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1399

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1478

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1479

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1491

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1497

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1514

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1520

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1536

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1562

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1655

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1663

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1674

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1676

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1679

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1713

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1714

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1757

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1770

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1794

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1811

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1878

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1880

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1901

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1907

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1925

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1938

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1940

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1944

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1955

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1956

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1958

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2065

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2081

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2082

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2113

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2120

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2149

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2211

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2236

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2247

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2281

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2297

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2303

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2308

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2382

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2426

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2429

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2438

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2496

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2501

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2576

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2588

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 139

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 183

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 517

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 570

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 881

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 979

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1049

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1140

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1217

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1222

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1450

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1479

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1583

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2080

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2141

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2580

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2594

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2612

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 33

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 64

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 94

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 139

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 151

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 183

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 197

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 235

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 243

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 250

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 323

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 331

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 361

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 446

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 458

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 480

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 497

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 510

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 517

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 536

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 552

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 570

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 685

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 703

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 717

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 730

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 791

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 808

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 881

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 892

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 920

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 946

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 965

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 979

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1045

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1049

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1115

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1140

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1158

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1161

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1190

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1203

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1206

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1217

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1222

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1237

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1240

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1370

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1385

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1412

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1450

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1467

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1473

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1475

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1479

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1499

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1554

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1574

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1583

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1586

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1631

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1640

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1655

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1665

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1683

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1713

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1723

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1725

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1739

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1762

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1766

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1769

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1802

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1811

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1830

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1849

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1871

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1916

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1963

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1967

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1976

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1999

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2003

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2036

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2059

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2080

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2101

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2118

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2141

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2214

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2216

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2289

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2318

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2337

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2378

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2390

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2398

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2406

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2430

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2453

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2494

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2511

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2535

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2580

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2594

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2602

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2612

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 295

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 341

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1195

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1417

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1465

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1866

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2176

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2253

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2472

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 99

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 128

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 175

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 254

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 270

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 296

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 337

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 341

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 352

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 388

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 416

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 438

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 491

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 530

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 558

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 616

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 629

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 635

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 659

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 671

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 696

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 779

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 786

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 836

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 854

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 858

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 885

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 894

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 991

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1015

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1033

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1051

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1055

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1058

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1059

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1067

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1077

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1104

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1111

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1116

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1122

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1126

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1134

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1145

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1156

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1194

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1195

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1197

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1236

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1240

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1241

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1279

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1287

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1308

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1323

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1326

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1329

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1337

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1344

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1354

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1378

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1390

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1391

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1417

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1449

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1465

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1488

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1492

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1497

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1538

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1542

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1547

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1562

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1568

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1598

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1613

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1633

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1654

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1662

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1755

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1837

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1863

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1864

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1866

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1870

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1882

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1902

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1913

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1945

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1951

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2030

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2098

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2142

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2163

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2176

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2180

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2244

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2253

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2306

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2337

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2346

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2347

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2348

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2472

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2503

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2528

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2540

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2558

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2589

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2600

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2617

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 815

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1225

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1411

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2447

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 14

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 19

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 38

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 63

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 73

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 98

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 123

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 143

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 149

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 154

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 160

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 186

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 188

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 278

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 280

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 292

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 294

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 301

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 387

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 396

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 415

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 527

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 548

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 613

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 674

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 680

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 708

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 721

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 744

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 765

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 798

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 807

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 815

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 841

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 854

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 859

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 867

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 879

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 882

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 890

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 897

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 970

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1024

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1033

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1053

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1078

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1090

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1111

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1164

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1218

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1225

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1250

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1253

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1256

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1289

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1329

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1376

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1382

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1394

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1411

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1418

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1445

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1583

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1647

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1649

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1703

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1722

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1795

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1797

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1819

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1842

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1845

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1853

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1856

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1880

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1894

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1918

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1953

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2002

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2009

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2023

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2070

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2090

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2110

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2115

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2128

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2139

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2159

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2185

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2233

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2262

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2277

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2324

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2447

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2493

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2512

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2524

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2534

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2540

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2546

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2578

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2585

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2590

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 169

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 195

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 354

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 665

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 700

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1002

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1397

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1408

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1410

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1423

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1713

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1827

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1927

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1971

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2264

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2284

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 11

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 48

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 52

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 68

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 84

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 131

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 169

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 188

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 195

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 211

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 216

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 262

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 271

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 297

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 354

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 384

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 413

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 445

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 471

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 497

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 514

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 523

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 524

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 544

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 652

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 665

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 672

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 685

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 700

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 774

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 783

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 791

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 798

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 813

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 896

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 937

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 996

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1002

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1027

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1063

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1064

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1112

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1187

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1201

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1244

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1245

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1269

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1281

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1328

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1332

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1341

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1370

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1371

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1397

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1402

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1408

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1410

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1423

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1430

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1454

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1491

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1500

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1513

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1575

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1631

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1651

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1655

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1692

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1694

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1701

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1713

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1730

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1827

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1848

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1861

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1912

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1927

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1948

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1958

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1971

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2036

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2185

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2186

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2192

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2219

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2264

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2272

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2284

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2327

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2456

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2458

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2491

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2521

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2523

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2541

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2544

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2552

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2585

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 792

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 820

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2430

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2463

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2566

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 26

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 38

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 72

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 74

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 138

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 143

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 168

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 169

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 175

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 191

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 194

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 251

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 305

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 359

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 374

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 377

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 393

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 493

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 501

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 506

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 575

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 583

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 604

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 613

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 670

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 708

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 754

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 770

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 792

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 820

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 895

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 915

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 936

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 970

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1048

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1072

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1096

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1138

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1161

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1165

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1174

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1178

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1186

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1192

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1236

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1245

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1267

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1301

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1325

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1329

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1335

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1338

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1368

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1369

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1430

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1442

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1495

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1502

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1536

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1658

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1659

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1675

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1822

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1839

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1865

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1878

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1887

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1918

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1955

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1960

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2101

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2110

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2119

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2129

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2187

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2202

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2205

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2211

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2251

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2276

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2295

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2308

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2367

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2381

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2389

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2430

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2436

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2463

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2487

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2494

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2545

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2566

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2616

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 137

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 398

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 619

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 739

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1248

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1626

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1887

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1900

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2495

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 37

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 49

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 88

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 122

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 137

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 228

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 295

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 346

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 392

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 398

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 428

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 447

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 455

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 456

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 489

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 504

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 513

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 524

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 550

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 564

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 584

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 619

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 666

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 673

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 676

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 739

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 754

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 761

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 766

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 799

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 806

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 840

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 860

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 889

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 913

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 935

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 964

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 998

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1028

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1042

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1054

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1066

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1161

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1166

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1175

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1192

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1219

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1248

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1274

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1288

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1293

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1303

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1330

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1352

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1367

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1377

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1388

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1473

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1516

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1530

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1589

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1611

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1612

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1623

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1626

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1648

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1649

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1651

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1726

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1738

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1755

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1774

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1785

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1842

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1871

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1887

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1889

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1900

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1920

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1930

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1936

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1956

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1967

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2013

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2033

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2050

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2091

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2108

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2131

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2158

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2187

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2215

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2274

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2308

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2312

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2340

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2437

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2495

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2514

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2525

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2531

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2547

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2597

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2605

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 61

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 85

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 119

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 153

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 159

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 202

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 213

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 218

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 230

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 279

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 285

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 298

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 304

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 322

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 372

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 450

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 491

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 518

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 540

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 554

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 572

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 577

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 584

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 687

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 693

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 700

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 719

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 747

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 765

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 824

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 842

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 849

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 864

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 885

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 893

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 928

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 940

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 971

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 989

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1002

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1009

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1055

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1069

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1105

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1112

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1121

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1195

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1209

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1237

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1253

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1260

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1294

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1301

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1304

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1372

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1406

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1423

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1428

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1437

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1445

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1448

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1496

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1516

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1524

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1533

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1535

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1551

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1579

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1605

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1658

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1680

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1746

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1794

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1800

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1804

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1822

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1840

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1846

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1848

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1852

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1875

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1903

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1905

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1915

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1920

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1952

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 1987

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2019

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2033

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2079

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2099

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2108

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2117

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2134

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2136

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2156

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2168

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2178

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2193

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2249

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2280

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2290

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2300

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2404

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2462

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2472

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2479

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2487

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2493

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2521

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2549

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2607

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2621

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2623

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2625

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2626

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2631

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2634

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2635

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2655

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2714

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2727

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2751

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2761

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2792

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2847

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2906

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2913

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2933

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2963

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2969

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 2990

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3007

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3023

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3034

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3040

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3042

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3059

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3083

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3092

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3114

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3126

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3154

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3156

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3162

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3169

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3178

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3260

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3274

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3293

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3327

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3404

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3450

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3466

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3511

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3539

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3569

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3675

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3695

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3752

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3811

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3817

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3850

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3854

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3904

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 3952

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4055

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4086

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4128

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4208

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4213

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4267

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4304

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4312

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4322

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4331

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4370

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4417

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4418

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4438

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4445

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4457

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4481

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4482

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4501

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4511

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4515

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4532

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4571

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4573

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4578

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4588

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4592

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4609

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4665

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4677

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4695

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4709

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4710

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4720

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4748

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4752

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4832

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4840

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4890

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4951

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4972

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 4999

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5004

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5086

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5126

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5141

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5204

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5233

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5255

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5263

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5266

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5268

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5296

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5314

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5318

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5422

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5466

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5475

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5491

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5493

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5505

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5513

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5629

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5681

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5699

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5779

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5819

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5858

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5878

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5927

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5940

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5983

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 5986

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6003

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6048

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6060

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6062

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6063

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6149

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6175

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6246

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6261

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6265

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6279

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6416

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6419

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6450

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6457

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6462

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6473

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6498

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6499

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6514

    ## Warning in FUN(X[[i]], ...): Numerical 0 probability for all classes with
    ## observation 6518

``` r
model
```

    ## Naive Bayes 
    ## 
    ## 26266 samples
    ##    11 predictor
    ##     6 classes: 'edm', 'latin', 'pop', 'r&b', 'rap', 'rock' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 23640, 23642, 23638, 23637, 23638, 23641, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   usekernel  Accuracy   Kappa    
    ##   FALSE      0.4489086  0.3379648
    ##    TRUE      0.3811405  0.2555036
    ## 
    ## Tuning parameter 'fL' was held constant at a value of 0
    ## Tuning
    ##  parameter 'adjust' was held constant at a value of 1
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were fL = 0, usekernel = FALSE
    ##  and adjust = 1.

``` r
xtabnb_other <- table(other_pred, nb_test[, 1])
confusionMatrix(xtabnb_other)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           
    ## other_pred edm latin pop r&b rap rock
    ##      edm   748   116 205  54 101  174
    ##      latin 103   438 168 181 197   75
    ##      pop   179   147 387 170 129  136
    ##      r&b    38   128 147 432 207  120
    ##      rap    71    88  66 154 460   17
    ##      rock   56    73 134 134  36  498
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4512          
    ##                  95% CI : (0.4391, 0.4633)
    ##     No Information Rate : 0.182           
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.3409          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: edm Class: latin Class: pop Class: r&b
    ## Sensitivity              0.6259       0.4424    0.34959    0.38400
    ## Specificity              0.8790       0.8702    0.86062    0.88240
    ## Pos Pred Value           0.5351       0.3769    0.33711    0.40299
    ## Neg Pred Value           0.9135       0.8979    0.86713    0.87389
    ## Prevalence               0.1820       0.1508    0.16857    0.17131
    ## Detection Rate           0.1139       0.0667    0.05893    0.06578
    ## Detection Prevalence     0.2129       0.1769    0.17481    0.16324
    ## Balanced Accuracy        0.7525       0.6563    0.60511    0.63320
    ##                      Class: rap Class: rock
    ## Sensitivity             0.40708     0.48824
    ## Specificity             0.92717     0.92194
    ## Pos Pred Value          0.53738     0.53491
    ## Neg Pred Value          0.88268     0.90738
    ## Prevalence              0.17207     0.15532
    ## Detection Rate          0.07005     0.07583
    ## Detection Prevalence    0.13035     0.14177
    ## Balanced Accuracy       0.66712     0.70509

``` r
getModelInfo(model = 'nb', regex = F)
```

    ## $nb
    ## $nb$label
    ## [1] "Naive Bayes"
    ## 
    ## $nb$library
    ## [1] "klaR"
    ## 
    ## $nb$loop
    ## NULL
    ## 
    ## $nb$type
    ## [1] "Classification"
    ## 
    ## $nb$parameters
    ##   parameter   class                label
    ## 1        fL numeric   Laplace Correction
    ## 2 usekernel logical    Distribution Type
    ## 3    adjust numeric Bandwidth Adjustment
    ## 
    ## $nb$grid
    ## function(x, y, len = NULL, search = "grid") 
    ##                     expand.grid(usekernel = c(TRUE, FALSE), fL = 0, adjust = 1)
    ## 
    ## $nb$fit
    ## function(x, y, wts, param, lev, last, classProbs, ...) {
    ##                    if(param$usekernel) {
    ##                           out <- klaR::NaiveBayes(x, y, usekernel = TRUE,  fL = param$fL, adjust = param$adjust, ...)
    ##                    } else out <- klaR::NaiveBayes(x, y, usekernel = FALSE, fL = param$fL, ...)
    ##                    out
    ##                   }
    ## 
    ## $nb$predict
    ## function(modelFit, newdata, submodels = NULL) {
    ##                     if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
    ##                     predict(modelFit , newdata)$class
    ##                   }
    ## 
    ## $nb$prob
    ## function(modelFit, newdata, submodels = NULL) {
    ##                     if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
    ##                     predict(modelFit, newdata, type = "raw")$posterior
    ##                     }
    ## 
    ## $nb$predictors
    ## function(x, ...) if(hasTerms(x)) predictors(x$terms) else x$varnames
    ## 
    ## $nb$tags
    ## [1] "Bayesian Model"
    ## 
    ## $nb$levels
    ## function(x) x$levels
    ## 
    ## $nb$sort
    ## function(x) x[order(x[,1]),]
