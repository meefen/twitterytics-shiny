library(shiny)

source("utils/utilities.R", local=TRUE)

EnsurePackage("ggplot2") # for ggplot, qplot
EnsurePackage("gdata") # for trim
EnsurePackage("xts") # for time series analysis
EnsurePackage("reshape") # for melt

source("utils/get_tweets.R")
source("utils/munge_tweets.R")
source("utils/semantic_analysis.R")

## load preprocessed data
load("data/df.Rda")
load("data/sentiments.Rda")
load("data/snaGraph.Rda")
load("data/snaMatrix.Rda")
load("data/communities.Rda")
load("data/corpus.Rda")
load("data/urls.Rda")

## read settings
settings <- read.table("settings.txt", header=FALSE)

## function for preparing ngram table
ngramTable <- function(query, user) {
  table <- data.frame(date=df$created_at, text=df$text, 
                      from_user=df$from_user, stringsAsFactors=FALSE)
  
  # count matches
  if(length(query) != 0) { # if tweet filter is not blank
    for(q in query) {
      # construct a frequency vector
      count <- unlist(lapply(table$text, function(tweet) 
        length(unlist(strsplit(tweet, q)))-1))
      # attach to a data frame
      table[[q]] <- count
    }
    
    if(length(query) > 1) {
      table$score <- apply(table[, 4:(3+length(query))], 1, sum)
    } else {
      table$score <- table[, ncol(table)]
    }
  } else { # if tweet filter is blank
    if(user == "") { # if both queries are blank, return the first 20 tweets
      table <- head(table, 20)
    }
    table$score <- 0.01
  }
  
  # filter user
  if(user != "") table <- table[grepl(user, table$from_user, ignore.case=TRUE), ]
  
  # sort
  table[with(table, order(-score)), ]
}

### Shiny Server ###

shinyServer(function(input, output) {
  
  # View count
  viewCount <- as.numeric(read.table("hits.txt", header=FALSE)[1, 1]) + 1
  write(viewCount, file = "hits.txt")
  
  #Output for hits
  output$hits <- renderText({
    paste0("App Hits: ", viewCount)
  })
  
  output$summary <- renderText({
    paste0("So far, ", length(unique(df$from_user)),
          " attendees have contributed ", nrow(df), 
          " tweets, between ", min(as.Date(df$created_at[!is.na(df$created_at)])), 
          " and ", max(as.Date(df$created_at[!is.na(df$created_at)])), ".")
  })
  
  ### 1. Tweets
  
  ## timeline
  output$tweets_timeline <- renderPlot({
    
    # Depending on the place of venue, change the timezone
    dates <- format(as.POSIXct(df$created_at, tz="GMT"), tz="America/Chicago")
    
    # trim dates to only keep date
    date.df <- data.frame(date=as.POSIXct(strftime(dates, "%Y-%m-%d")))
    summary <- ddply(date.df, .(date), summarise, freq=length(date))
    p <- ggplot(summary, aes(x=date, y=freq)) + geom_line() + geom_point()
    print(p)
  })
  
  ## render slider UI based on server side results
  output$numControls <- renderUI({
    if(input$rb_tweets == "urls") {
      countLinks <- getUrls()
      sliderInput("top_num", "", min=0, max=min(100, nrow(countLinks)), value=20, format = "#,##0")
    } else if(input$rb_tweets == "hashtags") {
      hashtags <- getHashtags()
      sliderInput("top_num", "", min=0, max=min(100, length(hashtags)), value=20, format = "#,##0")
    }
  })
  
  ## urls
  getUrls <- reactive({
    countLinks <- GetTweetCountTable(df, "links")
    names(countLinks)[1] <- "url"
    
    countLinks$count <- as.integer(countLinks$count)
    countLinks$url <- as.character(countLinks$url)
    return(countLinks)
  })
  # output table
  output$urls_table <- renderText({
    countLinks <- getUrls()
    countLinks.top <- head(countLinks, input$top_num)
    
    #### TODO: debug the following part online ###
    load("data/urls.Rda")
    # find longurl and title for each short url
    fullInfo <- data.frame(t(sapply(countLinks.top$url, function(x) {
      v <- as.vector(unlist(urls[which(urls$short == x), ])) # it might happen that no result
      if(length(v) == 0) return(rep(x, 3))
      return(v)
      # is found in urls (don't know why), so following code should take care of it
    })), stringsAsFactors=FALSE)
    names(fullInfo)[1] <- "url"
    
    # combine results of full info with count, by shorturl
    EnsurePackage("plyr")
    finalLinks <- join(countLinks.top, fullInfo, by="url")
    for(i in 1:nrow(finalLinks)) { ## deal with nonfound links
      if(is.na(finalLinks[i, 3]) || finalLinks[i, 3] == "") { ## nonfound
        finalLinks[i, 3] <- finalLinks[i, 1] ## not sure whether this will work
        finalLinks[i, 4] <- finalLinks[i, 1]
      }
    }
    
    #     ## or try the following
    #     finalLinks <- apply(finalLinks, 1, function(v) {
    #       if(v[2] == "") {
    #         c(rep(v[1], 3))
    #       } else {
    #         v
    #       }
    #     })
    
    df_rows <- apply(finalLinks, 1, link_row_html2)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH align="left">URLs</TH><TH align="left">Count</TH></TR>', collapse_cells, '</table>')
    
    return(full_table)
  })
  
  ## hashtags
  # reactive
  getHashtags <- reactive({
    hashtags <- as.vector(unlist(sapply(df$text, function(t) str_extract_all(t, "#\\S+"))))
    hashtags <- tolower(gsub("(?![#_])[[:punct:]]", "", hashtags, perl=TRUE))
    gsub("â|\u0080|\u009d|¦", "", hashtags, perl=TRUE)
  })
  # output table
  output$hashtags_table <- renderText({
    hashtags <- getHashtags()
    
    counts <- table(hashtags)
    counts <- data.frame(hashtag = unlist(dimnames(counts)),
                         count = as.integer(counts), 
                         row.names = NULL)
    counts <- counts[with(counts, order(-count, hashtag)), ]
    counts.sub <- head(counts, input$top_num)
    
    df_rows <- apply(counts.sub, 1, hashtag_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH align="left">Hashtags</TH><TH align="left">Count</TH></TR>', collapse_cells, '</table>')
    
    return(full_table)
  })
  
  ## sentiments
  # distribution plot
  output$sentiments <- renderPlot({
    # plot scores
    p <- ggplot(sentiments, aes(x=score)) + geom_histogram(binwidth=1) + 
      xlab("Sentiment score") + ylab("Frequency") + 
      ggtitle("Sentiment Analysis of Tweets")
    print(p)
  })
  # most happiest tweets
  output$happy_tweets <- renderText({
    df_rows <- apply(head(sentiments, input$sentiments_num), 1, tweet_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH align="left">Score</TH><TH align="left">Tweet</TH><TH align="left">User</TH></TR>', collapse_cells, '</table>')
    return(full_table)
  })
  # most saddest tweets
  output$sad_tweets <- renderText({
    df_rows <- apply(tail(sentiments, input$sentiments_num), 1, tweet_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH align="left">Score</TH><TH align="left">Tweet</TH><TH align="left">User</TH></TR>', collapse_cells, '</table>')
    return(full_table)
  })
  
  ## filter
  # conductor
  filterTweets <- reactive({
    queryStr <- tolower(trim(input$ngram_query))
    userStr <- tolower(trim(input$user_query))
    
    query <- trim(unlist(strsplit(queryStr, split="[, ]"))) # by comma or space
    query <- unique(query[query != ""]) # remove empty and repetitive elements
    
    ngramTable(query, userStr)
  })
  # plot ngram
  output$ngram_plot <- renderPlot({
    queryStr <- tolower(trim(input$ngram_query))
    if(queryStr == "") return # no ngram search
    
    query <- trim(unlist(strsplit(queryStr, split="[, ]"))) # by comma or space
    query <- unique(query[query != ""]) # remove empty and repetitive elements
    
    table <- filterTweets()
    
    ts <- xts(table[4:(ncol(table)-1)], table$date)
    ts.sum <- apply.daily(ts[, 1], sum)
    ts.sum.df <- data.frame(date=index(ts.sum), coredata(ts.sum))
    
    for(i in 2:length(query)) {
      if(i > length(query)) break # in case only one term
      ts.sum=apply.daily(ts[,i], sum)
      ts.sum.df1=data.frame(date=index(ts.sum), coredata(ts.sum))
      #colnames(ts.sum.df)=c("date", query[i])
      ts.sum.df <- cbind(ts.sum.df, ts.sum.df1[, 2])
      colnames(ts.sum.df)[length(ts.sum.df)] <- query[i]
    }
    
    ts.df <- melt(ts.sum.df, id="date")
    colnames(ts.df) <- c("date", "term", "frequency")
    p <- ggplot(ts.df, aes(date, frequency, colour=term)) + 
      geom_point() + geom_line() + xlim(min(df$created_at), max(df$created_at))
    
    print(p)
  })
  # table of tweets
  output$search_table <- renderText({
    table <- filterTweets()
    
    df_rows <- apply(table[table$score > 0, c("date", "text", "from_user", "score")], 1, search_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    
    if(max(table$score == 0.01)) { ## only filter by user
      full_table <- paste0('<table border="1"><TR><TH align="left">Date</TH><TH align="left">User</TH><TH align="left">Tweet</TH></TR>', collapse_cells, '</table>')
      return(full_table)
    } else {
      full_table <- paste0('<table border="1"><TR><TH align="left">Hits</TH><TH align="left">Date</TH><TH align="left">User</TH><TH align="left">Tweet</TH></TR>', collapse_cells, '</table>')
      return(full_table)
    }
  })
  
  
  ### 2. People
  
  ## counts by person
  # counts (conductor)
  countsTable <- reactive({
    countTweets <- GetTweetCountTable(df, "from_user")
    countRetweets <- GetTweetCountTable(df, "retweet_from")
    countReplies <- GetTweetCountTable(df, "reply_to")
    counts <- merge(countTweets, countRetweets, by="user", all.x=TRUE)
    counts <- merge(counts, countReplies, by="user", all.x=TRUE)
    colnames(counts) <- c("user", "tweets", "replied_to", "retweeted_by")
    counts$rt_ratio <- round(counts$retweeted_by/counts$tweets, 3)
    counts[is.na(counts)] <- 0
    
    counts$tweets <- as.integer(counts$tweets)
    counts$retweeted_by <- as.integer(counts$retweeted_by)
    counts$replied_to <- as.integer(counts$replied_to)
    
    counts <- counts[with(counts, order(-tweets, -replied_to, -retweeted_by, -rt_ratio)), ]
    row.names(counts) <- NULL
    counts
  })
  # render slider UI based on # of tweeters
  output$leaderSlider <- renderUI({
    counts <- countsTable()
    sliderInput("leaderCount", "", min=0, max=min(500,nrow(counts)), value=20, format = "#,##0")
  })
  # table
  output$counts_ppl <- renderText({
    counts <- countsTable()
    
    df_rows <- apply(head(counts, input$leaderCount), 1, leader_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH align="left">Tweeter</TH><TH align="left">Tweets</TH><TH align="left">Received Replies</TH><TH align="left">Be Retweeted</TH><TH align="left">Retweet Ratio</TH></TR>', collapse_cells, '</table>')
    return(full_table)
  })
  # plot
  output$counts_ppl_plot <- renderPlot({
    counts <- head(countsTable(), input$leaderCount)[, 1:4] # only visualize top 20 users
    counts$user <- factor(counts$user, levels=counts$user)
    counts.melt <- melt(counts, id.vars=c("user"))
    counts.melt$value <- as.numeric(counts.melt$value)
    
    # plot (Cleveland dot plot)
    p <- ggplot(counts.melt, aes(x=user, y=value, color=variable)) + 
      geom_point(position = position_jitter(width = .1), shape=15) + 
      coord_flip() + ggtitle("Counts of tweets, retweets, and messages") + 
      xlab("Users") + ylab("Counts")
    print(p)
  })
  
  ## sna_plot
  output$sna_plot <- renderPlot({
    EnsurePackage("sna")
    # plot with sna
    # TODO: distinguish colors by link types
    gplot(mat.csr)
  })
  
  ## sna table
  output$sna_stats <- renderTable({
    EnsurePackage("igraph")
    measures <- c("Nodes", "Edges", "Density", "Diameter", 
                  "Reciprocity", "Transitivity", "Degree Centralization", 
                  "Average Path Length", "Average Weighte Degree",
                  "Number of communities", "Size of largest community", "Modularity")
    numbers <-c(as.numeric(length(V(g))), 
                as.numeric(length(E(g))),
                as.numeric(graph.density(g)), 
                as.numeric(diameter(g)),
                as.numeric(reciprocity(g)),
                as.numeric(transitivity(g)),
                centralization.degree(g)$centralization,
                as.numeric(average.path.length(g, unconnected=FALSE)),
                as.numeric(mean(graph.strength(g))),
                length(g.wc),
                max(sizes(g.wc)),
                round(max(g.wc$modularity), 2))
    data.frame(measures, values=round(numbers, 4))
  }, include.rownames=FALSE)
  
  ## download csv
  output$downloadData <- downloadHandler(
    filename = function() { "data/df.csv" },
    content = function(file) {
      write.csv(df, file)
    }
  )
  
})
