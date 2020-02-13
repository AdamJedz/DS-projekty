none
================

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## <U+221A> ggplot2 3.2.1     <U+221A> purrr   0.3.3
    ## <U+221A> tibble  2.1.3     <U+221A> dplyr   0.8.4
    ## <U+221A> tidyr   1.0.2     <U+221A> stringr 1.4.0
    ## <U+221A> readr   1.3.1     <U+221A> forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   track_id = col_character(),
    ##   track_name = col_character(),
    ##   track_artist = col_character(),
    ##   track_album_id = col_character(),
    ##   track_album_name = col_character(),
    ##   track_album_release_date = col_character(),
    ##   playlist_name = col_character(),
    ##   playlist_id = col_character(),
    ##   playlist_genre = col_character(),
    ##   playlist_subgenre = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
df %>% head()
```

    ## # A tibble: 6 x 23
    ##   track_id track_name track_artist track_popularity track_album_id
    ##   <chr>    <chr>      <chr>                   <dbl> <chr>         
    ## 1 6f807x0~ I Don't C~ Ed Sheeran                 66 2oCs0DGTsRO98~
    ## 2 0r7CVbZ~ Memories ~ Maroon 5                   67 63rPSO264uRjW~
    ## 3 1z1Hg7V~ All the T~ Zara Larsson               70 1HoSmj2eLcsrR~
    ## 4 75Fpbth~ Call You ~ The Chainsm~               60 1nqYsOef1yKKu~
    ## 5 1e8PAfc~ Someone Y~ Lewis Capal~               69 7m7vv9wlQ4i0L~
    ## 6 7fvUMiy~ Beautiful~ Ed Sheeran                 67 2yiy9cd2QktrN~
    ## # ... with 18 more variables: track_album_name <chr>,
    ## #   track_album_release_date <chr>, playlist_name <chr>, playlist_id <chr>,
    ## #   playlist_genre <chr>, playlist_subgenre <chr>, danceability <dbl>,
    ## #   energy <dbl>, key <dbl>, loudness <dbl>, mode <dbl>, speechiness <dbl>,
    ## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
    ## #   tempo <dbl>, duration_ms <dbl>
