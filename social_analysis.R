
CreateSNADataFrame <- function(df, from, to, linkNames) {
  # Create SNA data frame
  #
  # Args:
  #   df: data frame containing raw data
  #   from: name of "from" column
  #   to: vector of names of "to" columns
  #   linkNames: vector of link names (e.g., retweet, reply)
  #
  # Note: I start with implementing 1-1 links
  
  EnsurePackage("plyr")
  
  df.sna <- data.frame(from = df[[from]], 
                       to = df[[to]], 
                       link = linkNames)
  # remove rows with NA
  df.sna <- na.omit(unique(df.sna))
  
  # merge rows with same metadata, and compute weight
  ddply(df.sna, .(from, to, link), summarise, weight=length(from))
}