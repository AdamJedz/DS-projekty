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

<<<<<<< HEAD
table(pred)
table(test_labels)
conf


#knn after normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

lim_train_n <- mydata_train %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)


lim_test_n <- mydata_test %>% select(playlist_genre, track_popularity, danceability, energy, key, loudness, mode, acousticness, instrumentalness, liveness, valence, tempo)

train_labels <- lim_train_n$playlist_genre
test_labels <- lim_test_n$playlist_genre  

head(lim_train_n)
lim_train_n <- normalize(lim_train_n[-1])
#lim_train_n <- lim_train_n %>% mutate(playlist_genre = NULL)

head(lim_test_n)
lim_test_n <- normalize(lim_test_n[-1])
#lim_test_n <- lim_test_n %>% mutate(playlist_genre = NULL)


k=65
pred <- knn(train = lim_train_n, test = lim_test_n, cl = train_labels, k = k)
conf <- table(test_labels, pred)  
accs[k] <- sum(diag(conf)) / sum(conf)
accs[k]


conf

#normalizacja danych psuje model, obnizajac acc do 18%
#większosc utworow przypisuje do r&b, rap, rock 
