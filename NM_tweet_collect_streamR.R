library(ROAuth)
library(streamR)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"

consumerKey <- "ax7CjhRloZz1iuQzAuBEsCgA8"
consumerSecret <- "BWSM1JEg3Oo4jV0OjgBKXXHj1YsE1If9gvXKnjSGuwk7Ah0qC4"

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
my_oauth$handshake()
current.time <- format(Sys.time(), "%Y-%m-%d-%H-%M")

f <- paste0("collect_NewMexico.", current.time, ".json")
keywords = NULL
location = c(-109.044800, 31.335184, -102.996826, 36.985588)

tweets <- filterStream( file.name=f,
                        locations=location, 
                        track = keywords, 
                        timeout=600, 
                        oauth=my_oauth )

