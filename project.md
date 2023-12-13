Analyzing Spotify Top Songs 2000-2019 to help increase youtube views for
aspiring instrumental cover youtubers Using R
================
12/12/2023

# Project Inspiration

I have been learning the guitar for a decade and it has always been a
passion of mine to share my own guitar cover videos of songs/pieces that
I like. I was always in awe to discover the talents on youtube and even
found it quite astonishing that some youtubers could gain a lot of views
from youtube videos by just playing instruments. This showed people
really appreciated true talet. Therefore, I am working on this project
to try to analyze what upcoming music will be popular to help musicians
to pick songs to cover to increase their popularity. To make data-driven
recommendations, I set and address some questions using data analysis
process: ask, prepare, process, analyze, share and act.

# *ASK*

### Scenario

By learning from past data of listener’s preference on Spotify, my task
is to analyze data on Spotify and extracting insights from popular music
to inform youtubers about decisions on instrumental covers. I have come
up with the following questions for my analysis:

1.  What are the general characteristics of popular song hits ?
2.  Which song key (major/minor) tend to lead to higher popularity ?
3.  What key do popular songs tend to be in ?

# *PREPARE*

The dataset about top hits from 2000-2019 is pulled from Kaggle. The
details are as follows:

**Name:** Top Hits Spotify from 2000-2019  
**Credit:** MARK KOVERHA  
**Source:** Spotify  
**Link:**
<https://www.kaggle.com/datasets/paradisejoy/top-hits-spotify-from-20002019/data>  
**Format:** CSV file, 18 columns, 2000 rows  
**Metadata:**  
- artist, song, duration_ms, explicit, year tells us some basic
information of the song.  
- popularity: higher value refers to higher popularity  
- genre: genre of the song  
- danceability, energy, key, loudness,mode, speechiness, acousticness,
instrumentalness, liveness, valence, tempo: music characterics of the
song  

# *PROCESS*

``` r
# load_packages
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(ggplot2)
```

``` r
spotify_songs <- read.csv("songs_normalize.csv")
tibble(spotify_songs)
```

    ## # A tibble: 2,000 × 18
    ##    artist  song  duration_ms explicit  year popularity danceability energy   key
    ##    <chr>   <chr>       <int> <chr>    <int>      <int>        <dbl>  <dbl> <int>
    ##  1 Britne… Oops…      211160 False     2000         77        0.751  0.834     1
    ##  2 blink-… All …      167066 False     1999         79        0.434  0.897     0
    ##  3 Faith … Brea…      250546 False     1999         66        0.529  0.496     7
    ##  4 Bon Jo… It's…      224493 False     2000         78        0.551  0.913     0
    ##  5 *NSYNC  Bye …      200560 False     2000         65        0.614  0.928     8
    ##  6 Sisqo   Thon…      253733 True      1999         69        0.706  0.888     2
    ##  7 Eminem  The …      284200 True      2000         86        0.949  0.661     5
    ##  8 Robbie… Rock…      258560 False     2000         68        0.708  0.772     7
    ##  9 Destin… Say …      271333 False     1999         75        0.713  0.678     5
    ## 10 Modjo   Lady…      307153 False     2001         77        0.72   0.808     6
    ## # ℹ 1,990 more rows
    ## # ℹ 9 more variables: loudness <dbl>, mode <int>, speechiness <dbl>,
    ## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
    ## #   tempo <dbl>, genre <chr>

``` r
#check if there is any duplicated data
spotify_songs_unique <- spotify_songs %>% distinct()
x <- nrow(spotify_songs) - nrow(spotify_songs_unique)
paste(x, "rows of data were duplicated and were removed from the dataset")
```

    ## [1] "59 rows of data were duplicated and were removed from the dataset"

``` r
#check if there is any rows with missing information
spotify_songs_clean <- spotify_songs_unique[!apply(is.na(spotify_songs_unique) | spotify_songs_unique == "", 1, all),]
empty <- nrow(spotify_songs_unique) - nrow(spotify_songs_clean)
paste(empty, "rows of data were empty or stated not applicable and were removed from the dataset")
```

    ## [1] "0 rows of data were empty or stated not applicable and were removed from the dataset"

``` r
write.csv(spotify_songs_clean, file='spotify_cleaned.csv')
```

# Analyze

First of all, the mean, min value, max value and standard deviation were
found for the attributes to learn about the general characterics of the
top hits in this dataset. The general characterictics can give a hint of
the metrics of an average popular song.

``` r
data <- read.csv("spotify_cleaned.csv")
#popularity
popularity_min <- min(data$popularity)
popularity_max <- max(data$popularity)
popularity_mean <- mean(data$popularity)
popularity_sd <- sd(data$popularity)
#danceability
danceability_min <- min(data$danceability)
danceability_max <- max(data$danceability)
danceability_mean <- mean(data$danceability)
danceability_sd <- sd(data$danceability)
#energy
energy_min <- min(data$energy)
energy_max <- max(data$energy)
energy_mean <- mean(data$energy)
energy_sd <- sd(data$energy)
#key
key_min <- min(data$key)
key_max <- max(data$key)
key_mean <- mean(data$key)
key_sd <- sd(data$key)
#loudness
loudness_min <- min(data$loudness)
loudness_max <- max(data$loudness)
loudness_mean <- mean(data$loudness)
loudness_sd <- sd(data$loudness)
#mode
mode_min <- min(data$mode)
mode_max <- max(data$mode)
mode_mean <- mean(data$mode)
mode_sd <- sd(data$mode)
#speechiness
speechiness_min <- min(data$speechiness)
speechiness_max <- max(data$speechiness)
speechiness_mean <- mean(data$speechiness)
speechiness_sd <- sd(data$speechiness)
#acoustiness
acousticness_min <- min(data$acousticness)
acousticness_max <- max(data$acousticness)
acousticness_mean <- mean(data$acousticness)
acousticness_sd <- sd(data$acousticness)
#instrumentalness
instrumentalness_min <- min(data$instrumentalness)
instrumentalness_max <- max(data$instrumentalness)
instrumentalness_mean <- mean(data$instrumentalness)
instrumentalness_sd <- sd(data$instrumentalness)
#liveness
liveness_min <- min(data$liveness)
liveness_max <- max(data$liveness)
liveness_mean <- mean(data$liveness)
liveness_sd <- sd(data$liveness)
#valence
valence_min <- min(data$valence)
valence_max <- max(data$valence)
valence_mean <- mean(data$valence)
valence_sd <- sd(data$valence)
#tempo
tempo_min <- min(data$tempo)
tempo_max <- max(data$tempo)
tempo_mean <- mean(data$tempo)
tempo_sd <- sd(data$tempo)
#export table
summary_table <-data.frame(
    MIN=c(popularity_min, danceability_min, energy_min,key_min, loudness_min,mode_min,speechiness_min,acousticness_min,instrumentalness_min,liveness_min,valence_min, tempo_min),
    MAX=c(popularity_max,danceability_max,energy_max,key_max,loudness_max,mode_max,speechiness_max,acousticness_max,instrumentalness_max, liveness_max, valence_max, tempo_max ),
    MEAN=c(popularity_mean,danceability_mean,energy_mean,key_mean,loudness_mean, mode_mean,speechiness_mean, acousticness_mean, instrumentalness_mean,liveness_mean, valence_mean,tempo_mean),
    SD=c(popularity_sd,danceability_sd,energy_sd,key_sd, loudness_sd,mode_sd,speechiness_sd,acousticness_sd,instrumentalness_sd, liveness_sd,valence_sd,tempo_sd))
    rownames(summary_table)<-c("popularity","danceability","energy", "key", "loundess","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo")
print(summary_table)
```

    ##                          MIN     MAX         MEAN          SD
    ## popularity         0.0000000  89.000  59.63317877 21.50105276
    ## danceability       0.1290000   0.975   0.66781401  0.14060828
    ## energy             0.0549000   0.999   0.72154869  0.15287162
    ## key                0.0000000  11.000   5.36939722  3.61526989
    ## loundess         -20.5140000  -0.276  -5.51408243  1.93895046
    ## mode               0.0000000   1.000   0.55332303  0.49727664
    ## speechiness        0.0232000   0.576   0.10378259  0.09614784
    ## acousticness       0.0000192   0.976   0.12817265  0.17258382
    ## instrumentalness   0.0000000   0.985   0.01537248  0.08837137
    ## liveness           0.0215000   0.853   0.18172633  0.14090954
    ## valence            0.0381000   0.973   0.55296615  0.22084538
    ## tempo             60.0190000 210.851 120.15844204 26.99047454

``` r
write.csv(summary_table,file="summary_table.csv")
```

Now, we explore how different music characteristics influences
popularity. Here, we only look at songs that have high popularity. For
songs to have high popularity, they have to reach higher than 75. \#
Songs of high popularity

``` r
music_characteristics_analysis <- read.csv("spotify_cleaned.csv")
high_popularity <- music_characteristics_analysis %>%  filter(popularity>=75)
total_no_of_songs <- nrow(high_popularity)
paste("Number of songs with high popularity: ",total_no_of_songs)
```

    ## [1] "Number of songs with high popularity:  397"

# Major/ Minor key

``` r
ggplot(data=high_popularity)+
    geom_bar(mapping=aes(x=mode))
```

![](project_files/figure-gfm/music%20characteristics%20and%20popularity-1.png)<!-- -->

``` r
major_songs <- music_characteristics_analysis %>%  filter(popularity>=75 & mode ==1)
minor_songs <- music_characteristics_analysis %>%  filter(popularity>=75 & mode == 0)
no_of_major_songs <- nrow(major_songs)
no_of_minor_songs <- nrow(minor_songs)
pie_chart_data <-c(no_of_major_songs, no_of_minor_songs)
pie_chart_labels<-c("Major","Minor")
piepercent <- round(100* pie_chart_data/sum(pie_chart_data),1)
pie(pie_chart_data,labels = piepercent, main="Major and Minor key pie chart")
legend("topright",c("Major","Minor"), cex=0.5,fill=rainbow(length(pie_chart_data)))
```

![](project_files/figure-gfm/music%20characteristics%20and%20popularity-2.png)<!-- -->  
*Observation:*  
From the pie chart, it was shown that songs in major key are more likely
to be more popular than minor key from this dataset.

# Key

``` r
ggplot(data=high_popularity)+
    geom_bar(mapping=aes(x=key))
```

![](project_files/figure-gfm/key-1.png)<!-- -->

*Observation:*  
From the pie chart, it was shown that songs in key 1 (C#) are the most
likely to be popular while songs in key 3 (D#) are the least likely to
be popular.

# *SHARE*

From the above observations, I have made some findings about the popular
sings across 2000-2019. Generally, popular songs differ a lot in terms
of Spotify metrics. For songs to be deemed to have high popularity in
Spotify, they tend to be in *C#* and *major* key. This also gave some
information about how music cover songs can be arranged in to suit the
audience’s taste more so they can gain more views on Spotify.

# *ACT*

With this working hypothesis, it will be good to use these information
to examine the data this year on Spotify to see if the top hit songs
still follows this success formula. If this formula works, I believe
aspiring muscician can take this as a source of inspiration to help
build their fame on Spotify.
