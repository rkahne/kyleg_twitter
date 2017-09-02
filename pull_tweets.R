library(tidyverse)
library(rtweet)
library(jsonlite)
library(tidytext)
library(stringr)
library(magrittr)
library(ggalt)
library(forcats)
library(lubridate)
library(treemap)

# legislator twitter metadata pulled from twitter developer console -- couldn't figure out how to do it with `rtweet`
# This is the json output form the console.
ky_leg_json <- read_json('ky_legislators.json')

# This data comes from the work done during the first Louisville Data for Democracy hackathon.
# It includes a bunch of info about legislators in 2016 -- party, gender, age (when available), etc.
legislators <- read_csv("https://query.data.world/s/UY947SPvDTwn5lyXSazhoNFC_pS2sv")

# I had to make a crosswalk by hand of legislators to their twitter handles.  By far, the most annoying part of the project.
crosswalk <- read_csv('twitter_db_crosswalk.csv')

# Function for getting tweets by handle.
# Maxes at 2500, but you can obviously adjust this.
get_ky_tweets <- function(screen_name){
  tryCatch({
    get_timeline(screen_name, n = 2500) %>% 
      filter(created_at >= '2017-01-01') 
  }, error = function(e){
    return('')
  })
}

# Pull out user name from ky_leg_json, append tweets using function above.
ky_leg_tweets <- tibble(
  user_name = map_chr(ky_leg_json$users, function(i) i$screen_name)
) %>% 
  mutate(tweets = map(user_name, get_ky_tweets))

# Join tweets to the crosswalk.  Inner join used to ensure only current legislators.
ky_leg_tweets %<>% 
  inner_join(crosswalk, by = c('user_name' = 'twitter_screenname')) %>% 
  left_join(legislators)

save(ky_leg_tweets, file = 'ky_leg_tweets.rda')