# Methods of retrieving tweets

# TODO:
# 1) metadate structures of different sources are different
#    need to make them adopt a same structure
#    refer to https://dev.twitter.com/docs/platform-objects/tweets

source("utils/utilities.R")

GetTweetsBySearch <- function(term, n = 1500) {
  # Get tweets by searching Twitter API
  # 
  # Args: 
  #   term: search term (e.g., #education)
  #   n: max number of tweets
  #
  # Returns:
  #   Data frame containing tweets
  
  EnsurePackage("twitteR")
  EnsurePackage("RCurl")
  EnsurePackage("bitops")
  EnsurePackage("rjson")
  
  # get tweets, and put in a df
  results <- searchTwitter(term, n)
  df <- do.call("rbind", lapply(results, as.data.frame))
  
  # rename metadata
  names.twitteR <- c("screenName", "created") # change from
  names.api <- c("screen_name", "created_at") # change to
  for(name in names.twitteR) {
    names(df)[which(names(df)==name)] <- names.api[which(names.twitteR==name)]
  }
  df$from_user <- df$screen_name
  
  return(df)
}

GetTweetsFromCSV <- function(file) {
  # Get tweets from a csv file
  # 
  # Args: 
  #   file: file name, with path
  #
  # Returns:
  #   Data frame containing tweets
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  # may need addtional handling here
  return(df)
}

GetTweetsFromGoogleDrive <- function(key, gid = 82) {
  # Get tweets from Google Spreadsheet
  # For how to archive tweets in Google Spreadsheet, read:
  # http://mashe.hawksey.info/2013/02/twitter-archive-tagsv5/
  #
  # Args:
  #   key: file key
  #   gid: grid id of archive sheet
  #
  # Returns:
  #   Data frame containing tweets
  
  EnsurePackage("RCurl")
  
  url <- paste(sep="", 'https://docs.google.com/spreadsheet/pub?key=', key, 
               '&single=true&gid=', gid, '&output=csv')
  conn <- textConnection(getURL(url))
  df <- read.csv(conn, stringsAsFactors = FALSE)
  close(conn)
  
  # formatting
  df$created_at <- strptime(df$time, "%d/%m/%Y %H:%M:%S")
  df$geo_coordinates[df$geo_coordinates == ""] <- NA
  df$screen_name <- df$from_user
  
  return(df)
}