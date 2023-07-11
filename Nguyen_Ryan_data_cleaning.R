# Loading Packages
library(tidyverse, quietly = T)
library(skimr, quietly = T)
library(lubridate)

billboard100 <- read_csv("data/unproccessed/billboard100.csv")
charts <- read_csv("data/unproccessed/charts.csv")
artistsDf <- read_csv("data/unproccessed/artistDf.csv")
grammy_albums <- read_csv("data/unproccessed/grammy_albums.csv")
grammy_songs <- read_csv("data/unproccessed/grammy_songs.csv")

## Spotify Charts
# having to use parse through the strings to clean the data
# removing track_id  (a track's unique Spotify id number), duration (duration of track in milliseconds), 
# and explicit (whether or not the track has explicit language) since we're focused on the artists' success

spotify_charts <- charts %>% 
  # removing unnecessary [''] around the artist and genres
  mutate(artists = str_remove_all(artists, pattern = "[\\[\\]']"),
         artist_genres = str_remove_all(artist_genres, pattern = "[\\[\\]']")) %>% 
  # removing irrelevant columns
  select(-c(track_id, duration, explicit))

skim(spotify_charts)
View(spotify_charts)

write.csv(spotify_charts, "spotify_charts.csv", quote = TRUE)

## Grammys

# grammy wins are separate based on albums and songs, but we want to see them together to see amount of total grammy wins, 
# need a full join to preserve all data

# start with tidying each data set individually
grammy_albums_tidy <- grammy_albums %>% 
  # renaming columns to be tidy
  rename(award = Award,
         name = Album,
         year = GrammyYear,
         genre = Genre,
         artist = Artist) %>% 
  # removing unnecessary columns
  select(-c(...1))

grammy_songs_tidy <- grammy_songs %>% 
  # renaming columns to be tidy
  rename(award = GrammyAward,
         name = Name,
         year = GrammyYear,
         genre = Genre,
         artist = Artist) %>% 
  # removing unnecessary columns
  select(-c(X, ...1))

skim(grammy_albums_tidy)

grammy_awards <- full_join(grammy_albums_tidy, grammy_songs_tidy)

write.csv(grammy_awards, "grammy_awards.csv", quote = TRUE)


# dates are characters, so we want to mutate them

billboard_hot <- billboard100 %>% 
  # mutating the date so it's usable
  mutate(Date = mdy(Date)) %>% 
  # renaming columns to be tidy
  rename(artist = Artists,
         name = Name,
         weekly_rank = Weekly.rank,
         peak_pos = Peak.position,
         weeks_on_chart = Weeks.on.chart,
         week = Week,
         date = Date,
         genre = Genre,
         writers = Writing.Credits,
         features = Features) %>% 
  # removing irrelevant columns, Lyrics are irrelevant here
  select(-c(...1, Lyrics))

View(billboard_hot)

write.csv(billboard_hot, "billboard_hot.csv", quote = TRUE)

skim(billboard100_tidy)

## Artists

artists_info <- artistsDf %>% 
  # renaming columns to be tidy
  rename(artist = Artist,
         followers = Followers,
         genre = Genres,
         num_albums = NumAlbums,
         debut = YearFirstAlbum,
         gender = Gender,
         type = Group.Solo) %>% 
  # removing irrelevant columns
  select(-c(X))

write.csv(artists_info, "artists_info.csv", quote = TRUE)


