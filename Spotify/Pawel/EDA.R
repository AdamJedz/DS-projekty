library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(ggthemes)


#create global theme to use later
my_theme <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text())

mydata_train <- read.csv("../train_set.csv", stringsAsFactors = FALSE, na.strings="")

mydata_test <- read.csv("../test_set.csv", stringsAsFactors = FALSE, na.strings="")

#union obu zbiorow
mydata <- bind_rows(mydata_train, mydata_test)

str(mydata)
head(mydata, n = 20) %>% kable() %>% kable_styling()
tail(mydata, n = 20)
summary(mydata)

#deleting id varaibles - no added value
colnames(mydata)
mydata <- mydata %>% select(-track_id, -track_album_id, -playlist_id)



#limited data - zmienna celu + zmienne wartościowe
my_limited <- mydata %>% select(playlist_genre, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)

str(my_limited)
summary(my_limited)



any(is.na(mydata)) # TRUE --> istnieja wartosci NA 

missing_data <- mydata %>% summarise_all(~(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
missing_data <- missing_data[missing_data$percent_missing > 0.0, ] 
missing_data  # missing (NA) values in 3 columns

ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing))  +
  geom_bar(stat = "identity", fill = "blue", size = 0.3) + xlab("Column name")

mydata %>% filter(is.na(track_name))



#1800 dates are not full dates --> change to year only
mydata$track_album_release_date <- substr(mydata$track_album_release_date, 1, 4)
str(mydata$track_album_release_date)
#mydata$track_album_release_date <- year(as.Date.POSIXct(mydata$track_album_release_date))

temp <- mydata %>% filter(track_album_release_date >= 1990) %>% group_by(track_album_release_date, playlist_genre) %>% summarize(album_count = n())
head(temp)



ggplot(temp, aes(x = track_album_release_date, y = album_count, color = playlist_genre, size = album_count)) +
  geom_point()
  











