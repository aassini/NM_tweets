library(tidyverse)
library(streamR)
library(readr)
library(dplyr)
oldw <- getOption("warn")
options(warn = -1)

# INPUT: make list of filename character strings
files <- list.files(path = getwd(), 
                    pattern="*.json", 
                    full.names=T, 
                    recursive=FALSE)

# Be sure to assign a new name!!!
# Otherwise, the new file will override the existing one
# under same name.
outnameEn <- '__Tw_mastersheet_En.csv'
outnameEs <- '__Tw_mastersheet_Es.csv'

# WHAT THIS FILTER ACHIEVES
# 
# From the more than 40 columns that Pablo's version of parseTweets() WANTS
# to read in, we retain only: screen_name, id_str, text, created_at, lang,
#                         user_lang, place_lat, place_lon, retweeted
# 
# It drops tweets that are retweeted = TRUE, and those that begin with 
# 'rt' or 'RT'.
# 
# It retains tweets that are from the bounding box for 
# TX: c(-106.568383, 26.890762, -94.131860, 36.481412).


### This first function is an edited, renamed version of parseTweets() 
### from the streamR package by Pablo Barbera. It speeds up the reading 
### by performing the read only for the columns that we want to retain.


leanParseTweets <- function(tweets, simplify=FALSE, verbose=TRUE){
  
  ## from json to list
  results.list <- readTweets(tweets, verbose=FALSE)
  
  # if no text in list, change it to NULL
  if (length(results.list)==0){
    stop(deparse(substitute(tweets)), " did not contain any tweets. ",
         "See ?parseTweets for more details.")
  }
  
  # constructing data frame with tweet and user variable
  df <- data.frame(
    text = unlistWithNA(results.list, 'text'),
    #retweet_count = unlistWithNA(results.list, 'retweet_count'),
    #favorite_count = unlistWithNA(results.list, 'favorite_count'),
    #favorited = unlistWithNA(results.list, 'favorited'),
    ##truncated = unlistWithNA(results.list, 'truncated'),
    id_str = unlistWithNA(results.list, 'id_str'),
    #in_reply_to_screen_name = unlistWithNA(results.list, 'in_reply_to_screen_name'),
    #source = unlistWithNA(results.list, 'source'),
    retweeted = unlistWithNA(results.list, 'retweeted'),
    created_at = unlistWithNA(results.list, 'created_at'),
    #in_reply_to_status_id_str = unlistWithNA(results.list, 'in_reply_to_status_id_str'),
    #in_reply_to_user_id_str = unlistWithNA(results.list, 'in_reply_to_user_id_str'),
    lang = unlistWithNA(results.list, 'lang'),
    #listed_count = unlistWithNA(results.list, c('user', 'listed_count')),
    #verified = unlistWithNA(results.list, c('user', 'verified')),
    #location = unlistWithNA(results.list, c('user', 'location')),
    user_id_str = unlistWithNA(results.list, c('user', 'id_str')),
    #description = unlistWithNA(results.list, c('user', 'description')),
    #geo_enabled = unlistWithNA(results.list, c('user', 'geo_enabled')),
    #user_created_at = unlistWithNA(results.list, c('user', 'created_at')),
    #statuses_count = unlistWithNA(results.list, c('user', 'statuses_count')),
    #followers_count = unlistWithNA(results.list, c('user', 'followers_count')),
    #favourites_count = unlistWithNA(results.list, c('user', 'favourites_count')),
    #protected = unlistWithNA(results.list, c('user', 'protected')),
    #user_url = unlistWithNA(results.list, c('user', 'url')),
    #name = unlistWithNA(results.list, c('user', 'name')),
    #time_zone = unlistWithNA(results.list, c('user', 'time_zone')),
    user_lang = unlistWithNA(results.list, c('user', 'lang')),
    #utc_offset = unlistWithNA(results.list, c('user', 'utc_offset')),
    #friends_count = unlistWithNA(results.list, c('user', 'friends_count')),
    screen_name = unlistWithNA(results.list, c('user', 'screen_name')),
    stringsAsFactors=F)
  
  # adding geographic variables and url entities
  if (simplify==FALSE){
    #df$country_code <- unlistWithNA(results.list, c('place', 'country_code'))
    #df$country <- unlistWithNA(results.list, c('place', 'country'))
    #df$place_type <- unlistWithNA(results.list, c('place', 'place_type'))
    #df$full_name <- unlistWithNA(results.list, c('place', 'full_name'))
    df$place_name <- unlistWithNA(results.list, c('place', 'name'))
    #df$place_id <- unlistWithNA(results.list, c('place', 'id'))
    place_lat_1 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 1, 2))
    place_lat_2 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 2, 2))
    df$place_lat <- sapply(1:length(results.list), function(x) 
      mean(c(place_lat_1[x], place_lat_2[x]), na.rm=TRUE))
    place_lon_1 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 1, 1))
    place_lon_2 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 3, 1))
    df$place_lon <- sapply(1:length(results.list), function(x) 
      mean(c(place_lon_1[x], place_lon_2[x]), na.rm=TRUE))
    #df$lat <- unlistWithNA(results.list, c('geo', 'coordinates', 1))
    #df$lon <- unlistWithNA(results.list, c('geo', 'coordinates', 2))
    #df$expanded_url <- unlistWithNA(results.list, c('entities', 'urls', 1, 'expanded_url'))
    #df$url <- unlistWithNA(results.list, c('entities', 'urls', 1, 'url'))
    
  }
  
  # information message
  if (verbose==TRUE) message(length(df$text), " tweets have been parsed.")
  return(df)
}


unlistWithNA <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3 & field[1]!="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (field[1]=="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]]))
  }
  
  if (length(field)==4 && field[2]!="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]][[field[3]]][[field[4]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]][[field[4]]]))
  }
  if (length(field)==4 && field[2]=="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (length(field)==6 && field[2]=="bounding_box"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) 
      x[[field[1]]][[field[2]]][[field[3]]][[as.numeric(field[4])]][[as.numeric(field[5])]][[as.numeric(field[6])]]))
  }
  return(vect)
}

### Pablo's function ends here


# open each file in files, try to leanParseTweets() it, 
# then rbind it to dfrm.
dfrm <- do.call(rbind,lapply(files, function(x){
  error <- tryCatch(tw <- leanParseTweets(x),
                    error = function(e) e)
  if (!inherits(error, 'error')){
    tw
  }
}
)
)

cat('\n\n\n\n\n\n\n\n\n\nlean-parsed', length(files), 'files')
cat('\nmade dataframe with', nrow(dfrm), 'rows and ', 
    ncol(dfrm), ' columns')

data <- dfrm
rm(dfrm)
# Remove retweets
# data <- data %>% filter(retweeted == FALSE)
# data <- data %>% filter(!grepl("^rt|^RT", text))
# if("retweeted" %in% names(data)){
#   data <- data %>% select(-retweeted)
# }

# Split data into English and Spanish tweets
dataEn <- data %>% filter(place_lat > 31.335184 
                          & place_lat < 36.985588 
                          & place_lon > -109.006348
                          & place_lon < -102.996826
                          # & user_lang == "en"
                          & lang == "en") %>%
  unique()

dataEs <- data %>% filter(place_lat > 31.335184 
                          & place_lat < 36.985588 
                          & place_lon > -109.006348
                          & place_lon < -102.996826
                          # & user_lang == "es"
                          & lang == "es") %>% 
  unique()

rm(data)


osizeEn <- format(object.size(dataEn), units='auto')
osizeEs <- format(object.size(dataEs), units='auto')
cat('\ndata frame size (English tweets):', osizeEn)
cat('\ndata frame size (Spanish tweets):', osizeEs)

# Write English data
write_csv(dataEn, outnameEn)
cat('\n\nwrote file', outnameEn, 'to disk\nit has', nrow(dataEn), 'rows and', ncol(dataEn), 'columns\n')
filesizeEn <- file.info(outnameEn)$size %>% utils:::format.object_size('auto')
cat('it has', filesizeEn, '\n\n')
rm(dataEn)

# Write Spanish data
write_csv(dataEs, outnameEs)
cat('wrote file', outnameEs, 'to disk\nit has', nrow(dataEs), 'rows and', ncol(dataEs), 'columns\n')
filesizeEs <- file.info(outnameEs)$size %>% utils:::format.object_size('auto')
cat('it has', filesizeEs, '\n\n')


# # finally, delete all the post/comment files
# for (i in c(1:length(files))){
#   f <- files[i]
#   file.remove(f)
# }

options(warn = oldw)
rm(list = ls())
