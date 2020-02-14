#Modelowanie

mydata_train <- read.csv("../train_set.csv", stringsAsFactors = FALSE, na.strings="")

mydata_test <- read.csv("../test_set.csv", stringsAsFactors = FALSE, na.strings="")

#limited data - zmienna celu + zmienne wartościowe
lim_train <- mydata_train %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)


lim_test <- mydata_test %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)


#KNN
