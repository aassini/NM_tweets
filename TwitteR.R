#setwd("/Users/SanchoPanza/Documents")
#source("TwitteR.R")

library(twitteR)

getTwitterOAuth(consumer_key, consumer_secret)
registerTwitterOAuth(oauth)

api_key <- "zuLW4SkOOcnNXaIGMMqFs42fk"
api_secret <- "pJHXNMCjwr86D6zYWFkAYdCW1ZJlTaS0zumtFTlDIUMY1ZWeFV"
access_token <- "499291112-FBri31f6ylnLhu4HoqFKjs6Yfs6VuK7yFifaO2wZ"
access_token_secret <- "Lf0HpI2iCknriWoIOoF74CV8KXrZVSnyrH1g5QY3XI4zr"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#install.packages("RWeka", type="source")
#install.packages("rJava", "http://rforce.net/", type="source")

#Sys.setenv(R_LIBS_USER= '/Users/SanchoPanza/Library/R/3.3/library/')
#Sys.setenv(JAVA_HOME= '/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/')
#Sys.setenv(LD_LIBRARY_PATH= '/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre/lib/server/')

#get into Applications: 
#sudo R CMD javareconf

#then install.packages

#install.packages("rJava","http://rforge.net/",type="source")

#then check:

# library(rJava)
# .jinit()
# .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
