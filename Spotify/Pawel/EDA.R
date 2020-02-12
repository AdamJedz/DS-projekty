library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)


mydata_train <- read.csv("../train_set.csv", stringsAsFactors = FALSE, na.strings="")

mydata_test <- read.csv("../test_set.csv", stringsAsFactors = FALSE, na.strings="")

#union obu zbiorow
mydata <- bind_rows(mydata_train, mydata_test)

str(mydata)
head(mydata, n = 20)
tail(mydata, n = 20)
summary(mydata)

#limited data - zmienna celu + zmienne wartościowe
my_limited <- mydata %>% select(playlist_genre, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)

str(my_limited)
summary(my_limited)



any(is.na(mydata)) # TRUE --> istnieja wartosci NA 

missing_data <- mydata %>% summarise_all(~(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
missing_data <- missing_data[missing_data$percent_missing > 0.0, ] 
missing_data  # missing (NA) values in 3 columns

mydata %>% filter(is.na(track_name))



#1800 dates are not full dates --> change to year only
mydata$track_album_release_date <- substr(mydata$track_album_release_date, 1, 4)
str(mydata$track_album_release_date)
#mydata$track_album_release_date <- year(as.Date.POSIXct(mydata$track_album_release_date))








