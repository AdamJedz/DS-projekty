library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(ggthemes)
library(corrplot)


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
my_limited <- mydata %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)

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


#checking number of track releases per genre over the years > 1990
temp <- mydata %>% filter(track_album_release_date >= 1990) %>% group_by(track_album_release_date, playlist_genre) %>% summarize(album_count = n())

ggplot(temp, aes(x = track_album_release_date, y = album_count, color = playlist_genre, size = album_count)) +
  geom_point()
  

quantile(mydata$track_popularity,seq(0,1,by=0.1))

#variables distribution per playlist_genre


#song count per genre
temp <- mydata %>% select(playlist_genre, track_popularity) %>% group_by(playlist_genre) %>% summarise(count = n())

ggplot(temp, aes(x = playlist_genre)) + geom_bar(fill = "red")


#popularity
temp <- mydata %>% select(playlist_genre, track_popularity) %>% group_by(playlist_genre)

ggplot(temp, aes(track_popularity)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of popularity") +
  geom_vline(xintercept = round(mean(temp$track_popularity), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)

#danceability
temp <- mydata %>% select(playlist_genre, danceability) %>% group_by(playlist_genre)

ggplot(temp, aes(danceability)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of danceability") +
  geom_vline(xintercept = round(mean(temp$danceability), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)


#energy
temp <- mydata %>% select(playlist_genre, energy) %>% group_by(playlist_genre)

ggplot(temp, aes(energy)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of energy") +
  geom_vline(xintercept = round(mean(temp$energy), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)

#key
temp <- mydata %>% select(playlist_genre, key) %>% group_by(playlist_genre)

ggplot(temp, aes(key)) +
  geom_histogram(bins = 10, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of key") +
  geom_vline(xintercept = round(mean(temp$key), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)


#loudness - due to outliers taking into account only values over 20th percentile
temp <- mydata %>% select(playlist_genre, loudness) %>% filter(loudness > quantile(mydata$loudness, 0.2)) %>%group_by(playlist_genre)
temp
ggplot(temp, aes(loudness)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of loudness") +
  geom_vline(xintercept = round(mean(temp$loudness), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre) 

#mode
mydata$mode
temp <- mydata %>% select(playlist_genre, mode) %>% group_by(playlist_genre)

ggplot(temp, aes(x = mode)) + geom_bar(fill = "red") +  
   my_theme  + ggtitle("Distribution of mode") +
  facet_wrap(~ playlist_genre)


#acousticness - big number of aoutliers + log10 + boxplot
quantile(mydata$acousticness,seq(0,1,by=0.1))

temp <- mydata %>% select(playlist_genre, acousticness) %>% group_by(playlist_genre)

ggplot(temp, aes(acousticness)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of acousticness") +
  geom_vline(xintercept = round(mean(temp$acousticness), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre) +scale_x_log10()


ggplot(temp, aes(x = playlist_genre, y = acousticness)) +
  geom_boxplot() +  
  my_theme  + ggtitle("Distribution of acousticness") 


#instrumentalness - only after log scaling we can distribution, most songs are not instrumental
quantile(mydata$instrumentalness,seq(0,1,by=0.1))

temp <- mydata %>% select(playlist_genre, instrumentalness) %>% group_by(playlist_genre)

ggplot(temp, aes(instrumentalness)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of instrumentalness") +
  geom_vline(xintercept = round(mean(temp$instrumentalness), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)

ggplot(temp, aes(instrumentalness)) +
  geom_histogram(bins = 5, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of instrumentalness") +
  geom_vline(xintercept = round(mean(temp$instrumentalness), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre) +scale_x_log10()




#liveness + log() scaling

temp <- mydata %>% select(playlist_genre, liveness) %>% group_by(playlist_genre)

ggplot(temp, aes(liveness)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of liveness") +
  geom_vline(xintercept = round(mean(temp$liveness), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre) +scale_x_log10()



#valence

temp <- mydata %>% select(playlist_genre, valence) %>% group_by(playlist_genre)

ggplot(temp, aes(valence)) +
  geom_histogram(bins = 50, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of valence") +
  geom_vline(xintercept = round(mean(temp$valence), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)


#tempo 
#strange outliers in edm
#excluding outliers

temp <- mydata %>% select(playlist_genre, tempo) %>% filter(tempo > quantile(tempo, 0.95)) %>% group_by(playlist_genre)

ggplot(temp, aes(tempo)) +
  geom_histogram(bins = 20, aes(y = ..density..), fill = "red") +  
  geom_density(alpha = 0.002, fill = "black") + 
  my_theme  + ggtitle("Distribution of tempo") +
  geom_vline(xintercept = round(mean(temp$tempo), 2), size = 1, linetype = 2) +
  facet_wrap(~ playlist_genre)

quantile(mydata$tempo,seq(0,1,by=0.1))


#checking correlation
#checking the correlation, used spearman as the data doesn't show big linearities

temp <- my_limited[complete.cases(my_limited), ]
temp <- temp[-1]


cor_mat <- cor(temp)
corrplot(cor_mat) 
#energy shows big positive correlation to loudness and rather big negative corr to acousticness. Perhaps some variables should be removed
