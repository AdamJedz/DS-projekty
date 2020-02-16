import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV

train_set = pd.read_csv('../train_set.csv')
train_set = train_set.rename(columns={'Unnamed: 0': 'index'}).set_index('index')
train_set.drop(['track_id', 'track_album_id', 'playlist_id'], axis = 1, inplace=True)
train_set = train_set[train_set['duration_ms'] >= 30000]

X_train, y_train = train_set.loc[:, ['track_popularity', 'danceability', 'energy', 'loudness', 'mode', 'speechiness', 'acousticness', 'instrumentalness',
          'liveness', 'valence', 'tempo']], train_set['playlist_genre']

params = {'n_estimators': [100, 250, 500, 1000],
         'max_depth': [None, 10, 20],
         #'min_samples_split': [2, 5, 10],
         #'min_samples_leaf': [1, 5, 10, 20],
         'max_features': [None, 'auto'],
         'random_state': [1]}



rf_clf = RandomForestClassifier()
clf = GridSearchCV(rf_clf, params, verbose=2, n_jobs=6)
clf.fit(X_train, y_train)
print(clf.best_score_)
print(clf.best_params_)
