source('utils/utilities.R')

EnsurePackage("stringr")

RemoveOddChars <- function(df) {
  # Remove odd characters in tweets
  
  df$text <- sapply(df$text, function(x) TrimOddChar(x))
  return(df)
}

ExtractUserInfo <- function(df) {
  # For each tweet, extract information related to users
  # such as to_user, rt_user...
  
  # extract to_user
  df$reply_to <- sapply(df$text, function(tweet) 
    TrimHead(str_extract(tweet,"^((\\.)?(@[[:alnum:]_]*))")))
  # extract rt_user
  df$retweet_from <- sapply(df$text, function(tweet) 
    TrimHead(str_extract(tweet,"^[RM]T (@[[:alnum:]_]*)")))
  
  return(df)
}

ExtractUrls <- function(df) {
  # For each tweet, extract url, remove it from the tweet,
  # and put them separately in a new column
  # TODO: cannot deal with multiple urls in one tweet right now
  
  EnsurePackage("stringr")
  EnsurePackage("grid")
  
  # extracts links (quick and dirty)
  # wish to have something like http://daringfireball.net/2009/11/liberal_regex_for_matching_urls
  df$links <- sapply(df$text,function(tweet) str_extract(tweet,("http[^[:blank:]]+")))
  df$links <- sub("â\u0080", "", df$links)
  df$links <- sub("¦", "", df$links)
  df$links <- sub("\u009d", "", df$links)
  df$links <- sub("\\.{3}", "", df$links)
  df$links[nchar(df$links) < 20] <- NA
  
  # unshorten links, and get titles of links
  #   df$longlinks <- sapply(df$links, function(l) UnshortenURL(l))
  #   df$linkTitle <- sapply(df$longlinks, GetTitleOfURL(l))
### commented out because longurl api is not working right now...
#   longlinks <- unlist(lapply(df$links, function(l) UnshortenURL2(l)))
#   while(length(longlinks) != 2 * length(df$links)) {
#     longlinks <- unlist(lapply(df$links, function(l) UnshortenURL2(l)))
#   }
#   df$longlinks <- longlinks[seq(1, length(longlinks), 2)]
#   df$linkTitle <- longlinks[seq(2, length(longlinks), 2)]
  
  # get a copy of text without urls for semantic analysis
  df$text_nourl <- sapply(df$text, function(x) TrimUrls(x))
  
  return(df)
}

PreprocessTweets <- function(df) {
  # Perform a few preprocessing tasks
  
  # removing odd characters
  df.new <- RemoveOddChars(df)
  # extract user info and add to df
  df.new <- ExtractUserInfo(df.new)
  # extract urls and add to df
  df.new <- ExtractUrls(df.new)
  
  return(df.new)
}

GetTweetCountTable <- function(df, col, threshold = 0) {
  # Count tweets for each user, 
  # sort the table in a decending order,
  # and filter users who posted less than the threshold
  
  counts <- table(df[, col])
  # create an ordered data frame
  counts <- data.frame(user = unlist(dimnames(counts)),
                       count = as.integer(counts), 
                       row.names = NULL)
  counts <- counts[with(counts, order(-count, user)), ]
  # create a subset of those who tweeted at least 5 times or more
  counts <- subset(counts, counts$count > threshold)
  return(counts)
}

GetURLCountTable <- function(df) {
  # Extract URLs from tweets and count them
  
  EnsurePackage(stringr)
  EnsurePackage(grid)
  
  # get frequencies of each link and put in rank order
  countLinks <- data.frame(url = as.character(unlist(dimnames(sort(table(df$links))))), 
                           count = sort(table(df$links)))
  rownames(countLinks) <- NULL # remove rownames
  countLinks$count <- as.integer(countLinks$count)
  
  return(countLinks)
}

AnonymizeUsers <- function(df) {
  # Anonymize users, by creating random numbers for each user
  #
  # Args:
  #   df: data frame of tweets
  #
  # Returns:
  #   new data frame with a new column containing ids
  
  # find out how many random numbers we need
  n <- length(unique(df$screenName))
  # generate a vector of random number to replace the names
  # we'll get four digits just for convenience
  randuser <- round(runif(n, 1000, 9999),0)
  # match up a random number to a username
  screenName <- unique(df$screenName)
  screenName <- sapply(screenName, as.character)
  randuser <- cbind(randuser, screenName)
  # merge the random numbers with the rest of the Twitter data
  # and match up the correct random numbers with multiple instances of the usernames
  rand.df  <-  merge(randuser, df, by="screenName")
  
  return(rand.df)
}