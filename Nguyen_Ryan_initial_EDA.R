library(tidyverse, quietly = T)
library(skimr, quietly = T)
library(lubridate, quietly = T)



skim(grammy_awards)

skim(artists_info)

top_songs <- spotify_charts %>% 
  filter(artists == "Taylor Swift"|artists =="Rihanna"|artists =="Drake") %>% 
  group_by(name, artists) %>% 
  slice_max(order_by = streams) %>% 
  group_by(artists) %>% 
  mutate(total_streams = sum(streams)) %>% 
  slice_head(n = 5)

top_songs %>%  
  ggplot(aes(x = fct_reorder(paste(artists, name, 
                                   sep = ' - '), streams), y = streams)) +
  geom_col(color = "black", fill = "lightblue") +
  coord_flip() +
  labs(title = "# Streams of Artist's Top 5 Most Streamed Songs on Spotify Charts", 
       y = "Number of Streams",
       x = "Artist - Song") 

spotify_charts %>% 
  filter(artists == "Taylor Swift"|artists =="Rihanna"|artists =="Drake") %>% 
  group_by(name, artists) %>% 
  slice_max(order_by = streams) %>% 
  group_by(artists) %>% 
  mutate(total_streams = sum(streams)) %>% 
  distinct(artists, total_streams) %>% 
  ggplot(aes(x= artists, y = total_streams)) +
  geom_col(color = "black", 
           fill = "lightblue") +
  labs(title = "Artist's Total # of Streams on Spotify Charts", 
       y = "Number of Streams",
       x = "Artist") 

spot <- spotify %>%
  group_by(artist, name) %>% 
  # finding unique artist & songs
  distinct(name, artist, streams, followers) %>% 
  # finding actual number of streams each song by each artist has
  slice_max(order_by = streams) %>% 
  group_by(artist) %>% 
  # adding artist's streams together
  mutate(total_streams = sum(streams)) %>% 
  distinct(artist, total_streams, followers)

View(spot)

usa_spot <- spotify_charts %>% 
  filter(country == "us")

View(usa_spot)

spotify_weeks <- spotify_charts %>%
  filter(country == "us") %>% 
  mutate(year = year(date)) %>% 
  group_by(artists, year) %>% 
  count() %>% 
  group_by(year) %>% 
  slice_max(order_by = n) %>% 
  rename("Artist Who Spent Longest on Spotify Charts" = "artists")

# songs that were on Billboard Hot 100 each year for the most amount of weeks
billboard_weeks <- billboard_hot %>% 
  mutate(year = year(week)) %>% 
  group_by(year) %>% 
  slice_max(order_by = weeks_on_chart) %>% 
  select(artist, name, year) %>% 
  distinct(artist) %>% 
  rename("Artist Who Spent Longest on Billboard Hot 100" = "artist")

spotify_billboard <- spotify_weeks %>% 
  left_join(billboard_weeks, "year") %>% 
  select(-c(n))

as_tibble(spotify_billboard)

streams_charts <- spotify_charts %>% 
  group_by(artists, name) %>% 
  # finding unique artist & songs
  distinct(name, artists, streams) %>% 
  # finding actual number of streams each song by each artist has
  slice_max(order_by = streams) %>% 
  # grouping artists together
  group_by(artists) %>% 
  # adding artist's streams together
  mutate(total_streams = sum(streams)) %>% 
  # selecting just artist and streams
  select(artists, total_streams) %>% 
  distinct(artists, total_streams)

  