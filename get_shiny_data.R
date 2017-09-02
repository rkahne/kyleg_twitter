library(tidyverse)
library(jsonlite)

ky_leg_json <- read_json('ky_legislators.json')
legislators <- read_csv("https://query.data.world/s/UY947SPvDTwn5lyXSazhoNFC_pS2sv")
crosswalk <- read_csv('twitter_db_crosswalk.csv')

load('ky_leg_tweets.rda')

ky_leg_follow <- tibble(
  user_name = map_chr(ky_leg_json$users, function(i) i$screen_name),
  followers = map_dbl(ky_leg_json$users, function(i) i$followers_count),
  following = map_dbl(ky_leg_json$users, function(i) i$friends_count)
) %>% 
  inner_join(crosswalk, by = c('user_name' = 'twitter_screenname')) %>% 
  left_join(legislators)

ky_leg_filtered <- filter(ky_leg_tweets, tweets != '')
save(ky_leg_filtered, file = 'kyleg_tweet_shiny.rda')