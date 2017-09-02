# Pulled almost directly from rtweet vignette: http://rtweet.info/

library(rtweet)

# For this to work you have to create a twitter application.
twitter_token <- create_token('KY Legislators App', read_csv('oauth.csv')$key,read_csv('oauth.csv')$secret)

## path of home directory
home_directory <- path.expand("~/")

## combine with name for token
file_name <- file.path(home_directory, "twitter_token.rds")

## save token to home directory
saveRDS(twitter_token, file = file_name)

# Add this to the PATH.
cat(paste0("TWITTER_PAT=", file_name),
    file = file.path(home_directory, ".Renviron"),
    append = TRUE)
