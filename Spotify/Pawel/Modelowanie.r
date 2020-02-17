#Modelowanie
library(class)


mydata_train <- read.csv("../train_set.csv", stringsAsFactors = FALSE, na.strings="")

mydata_test <- read.csv("../test_set.csv", stringsAsFactors = FALSE, na.strings="")

#limited data - zmienna celu + zmienne wartościowe
lim_train <- mydata_train %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)


lim_test <- mydata_test %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)

set.seed(12345)

#KNN
train_labels <- lim_train$playlist_genre
test_labels <- lim_test$playlist_genre  

lim_train$playlist_genre <- NULL
lim_test$playlist_genre <- NULL 

range <- 1:round(0.003 * nrow(lim_train))
accs <- rep(0, length(range))

for (k in range) {
  
  pred <- knn(train = lim_train, test = lim_test, cl = train_labels, k = k)
  conf <- table(test_labels, pred)  
  accs[k] <- sum(diag(conf)) / sum(conf)
  
}

plot(range, accs, xlab = "k")
which.max(accs)

#najwyzsze ACC dla k =65 
k=65
pred <- knn(train = lim_train, test = lim_test, cl = train_labels, k = k)
conf <- table(test_labels, pred)  
accs[k] <- sum(diag(conf)) / sum(conf)
accs[k]

pred

