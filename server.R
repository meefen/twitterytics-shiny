library(shiny)

source("utilities.R", local=TRUE)

EnsurePackage("ggplot2") # for ggplot, qplot
EnsurePackage("gdata") # for trim
EnsurePackage("xts") # for time series analysis
EnsurePackage("reshape") # for melt

source("get_tweets.R")
source("munge_tweets.R")
source("semantic_analysis.R")

load("data/aera13.Rda")
load("data/sentiments.Rda")
load("data/snaGraph.Rda")
load("data/snaMatrix.Rda")
load("data/communities.Rda")
load("data/corpus.Rda")

## function for preparing ngram table
ngramTable <- function(query, user) {
  table <- data.frame(date=df$created_at, text=df$text, 
                      from_user=df$from_user, stringsAsFactors=FALSE)
  
  # count matches
  if(length(query) != 0) {
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
  } else {
    table$score <- 0.01
  }
  
  # filter user
  if(user != "") table <- table[grepl(user, table$from_user, ignore.case=TRUE), ]
  
  # sort
  table[with(table, order(-score)), ]
}

## functions for building html tables
# a cell
cell_html <- function(table_cell) {
  paste0('<td>', table_cell, '</td>')
}
# a cell with link
linkCell_html <- function(table_cell) {
  paste0('<td><a href="', table_cell, '" target="_blank">', 
         table_cell, '</a></td>')
}
hash_LinkCell_html <- function(hash_cell) {
  paste0('<td><a href="https://twitter.com/search?q=%23', 
         substr(hash_cell, 2, nchar(hash_cell)), 
         '" target="_blank">', hash_cell, '</a></td>')
}
user_LinkCell_html <- function(user_cell) {
  paste0('<td><a href="https://twitter.com/', 
         user_cell, '" target="_blank">', user_cell, '</a></td>')
}
# a row for link table
link_row_html <- function(table_row) {
  collapse_cells <- paste0(linkCell_html(table_row[1]), 
                           cell_html(table_row[2]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}
# a row for hashtag table
hashtag_row_html <- function(table_row) {
  collapse_cells <- paste0(hash_LinkCell_html(table_row[1]), 
                           cell_html(table_row[2]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}
# a row for sentiment tweet
tweet_row_html <- function(table_row) {
  collapse_cells <- paste0(cell_html(table_row[1]), 
                           cell_html(table_row[2]), 
                           user_LinkCell_html(table_row[3]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}
# a row for search result tweet
search_row_html <- function(table_row) {
  hit <- as.numeric(table_row[4])
  if(hit == 0.01) { ## only filter by user
    collapse_cells <- paste0(cell_html(as.character(table_row[1])), 
                             user_LinkCell_html(table_row[3]), 
                             cell_html(table_row[2]), 
                             collapse='')
    paste0('<tr>', collapse_cells, '</tr>')
  } else {
    collapse_cells <- paste0(cell_html(table_row[4]), 
                             cell_html(as.character(table_row[1])), 
                             user_LinkCell_html(table_row[3]), 
                             cell_html(table_row[2]), 
                             collapse='')
    paste0('<tr>', collapse_cells, '</tr>')
  }
}
# a row for leader tweeter
leader_row_html <- function(row) {
  collapse_cells <- paste0(user_LinkCell_html(row[1]), 
                           cell_html(row[2]), 
                           cell_html(row[3]), 
                           cell_html(row[4]), 
                           cell_html(row[5]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

shinyServer(function(input, output) {
  
  output$summary <- renderText({
    paste("So far, ", length(unique(df$from_user)),
          "attendees have contributed ", nrow(df), 
          "tweets, between ", min(df$time[df$time!=""]), 
          " and ", max(df$time), ".")
  })
  
  ### 1. Tweets
  
  ## timeline
  output$tweets_timeline <- renderPlot({
    # trim dates to only keep date
    date.df <- data.frame(date=as.POSIXct(strftime(df$created_at, "%Y-%m-%d")))
    summary <- ddply(date.df, .(date), summarise, freq=length(date))
    p <- ggplot(summary, aes(x=date, y=freq)) + geom_line() + geom_point()
    print(p)
  })
  
  ## render slider UI based on server side results
  output$numControls <- renderUI({
    if(input$rb_tweets == "urls") {
      countLinks <- getUrls()
      sliderInput("top_num", "", min=0, max=nrow(countLinks), value=20, format = "#,##0")
    } else if(input$rb_tweets == "hashtags") {
      hashtags <- getHashtags()
      sliderInput("top_num", "", min=0, max=length(hashtags), value=20, format = "#,##0")
    }
  })
  
  ## urls
  getUrls <- reactive({
    countLinks <- GetTweetCountTable(df, "links")
    names(countLinks)[1] <- "url"
    
    countLinks$count <- as.integer(countLinks$count)
    countLinks$url <- as.character(countLinks$url)
    # remove those short urls (FIXME: need to fix url regexp)
    good <- as.vector(sapply(countLinks$url, function(l) nchar(l) >= 20))
    countLinks <- countLinks[good, ]
    row.names(countLinks) <- NULL
    return(countLinks)
  })
  # output table
  output$urls_table <- renderText({
    countLinks <- getUrls()
    countLinks.top <- head(countLinks[with(countLinks, order(-count)), ], input$top_num)
    
    # TODO: unshorten URLs and get titles
    
#     library(RCurl)
#     base_url <- "http://t.co/CtFRApQPQb"
#     decode_short_url(countLinks.top$url)
#     decode_short_url("http://tinyurl.com/adcd",
#                      "http://www.google.com")
#     base_html <- getURLContent(base_url)[[1]]
#     title <- regexec("<title>(.*)</title>", base_html)
#     if(length(title[[1]]) == 1) title <- regexec("<h1>(.*)</h1>", base_html)
#     substr(base_html, title[[1]][2], title[[1]][2] + attr(title[[1]],"match.length")[2] - 1)
    
    df_rows <- apply(countLinks.top, 1, link_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH>URLs</TH><TH>Count</TH></TR>', collapse_cells, '</table>')
    
    return(full_table)
  })
  
  ## hashtags
  # reactive
  getHashtags <- reactive({
    hashtags <- as.vector(unlist(sapply(df$text, function(t) str_extract_all(t, "#\\S+"))))
    tolower(gsub("(?![#_])[[:punct:]]", "", hashtags, perl=TRUE))
  })
  # output table
  output$hashtags_table <- renderText({
    hashtags <- getHashtags()
    
    counts <- table(hashtags)
    counts <- data.frame(hashtag = unlist(dimnames(counts)),
                         count = as.integer(counts), 
                         row.names = NULL)
    counts <- counts[with(counts, order(-count, hashtag)), ]
    
    df_rows <- apply(head(counts, input$top_num), 1, hashtag_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH>Hashtags</TH><TH>Count</TH></TR>', collapse_cells, '</table>')
    
    return(full_table)
  })
  
#   ## wordcloud
#   output$wordcloud <- renderPlot({
#     MakeWordCloud(corpus)
#   })
  
  ## topics
  #   output$topics <- renderTable({
  #     td.mat <- TermDocumentMatrix(corpus, control=list(minWordLength=3))
  #     lda <- TrainLDAModel(td.mat)
  #     lda_terms <- get_terms(lda, 5)
  #     lda_terms[, 1:5]
  #   })
  
  ## mds
  #   output$mds <- renderPlot({
  #     td.mat <- as.matrix(TermDocumentMatrix(corpus))
  #     dist.mat <- dist(t(as.matrix(td.mat)))
  #     fit <- cmdscale(dist.mat, eig=TRUE, k=2)
  #     points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
  #     p <- ggplot(points, aes(x=x,y=y)) + 
  #       geom_point(data=points,aes(x=x, y=y, color=df$from_user))
  #     print(p)
  #   })
  
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
    full_table <- paste0('<table border="1"><TR><TH>Score</TH><TH>Tweet</TH><TH>User</TH></TR>', collapse_cells, '</table>')
    return(full_table)
  })
  # most saddest tweets
  output$sad_tweets <- renderText({
    df_rows <- apply(tail(sentiments, input$sentiments_num), 1, tweet_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH>Score</TH><TH>Tweet</TH><TH>User</TH></TR>', collapse_cells, '</table>')
    return(full_table)
  })
  
  ## ngram
  filterTweets <- reactive({
    queryStr <- tolower(trim(input$ngram_query))
    userStr <- tolower(trim(input$user_query))
    cat(queryStr)
    cat(userStr)
    
    if(queryStr == "" && userStr == "") return # blank entry
    
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
  
  output$search_table <- renderText({
    table <- filterTweets()
    
    df_rows <- apply(table[table$score > 0, c("date", "text", "from_user", "score")], 1, search_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    
    if(max(table$score == 0.01)) { ## only filter by user
      full_table <- paste0('<table border="1"><TR><TH>Date</TH><TH>User</TH><TH>Tweet</TH></TR>', collapse_cells, '</table>')
      return(full_table)
    } else {
      full_table <- paste0('<table border="1"><TR><TH>Hits</TH><TH>Date</TH><TH>User</TH><TH>Tweet</TH></TR>', collapse_cells, '</table>')
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
  # render UI
  output$leaderSlider <- renderUI({
    counts <- countsTable()
    sliderInput("leaderCount", "", min=0, max=nrow(counts), value=20, format = "#,##0")
  })
  # table
  output$counts_ppl <- renderText({
    counts <- countsTable()
    
    df_rows <- apply(head(counts, input$leaderCount), 1, leader_row_html)
    collapse_cells <- paste0(df_rows, collapse='')
    full_table <- paste0('<table border="1"><TR><TH>Tweeter</TH><TH>Tweets</TH><TH>Received Replies</TH><TH>Be Retweeted</TH><TH>Retweet Ratio</TH></TR>', collapse_cells, '</table>')
    return(full_table)
  })
  # plot
  output$counts_ppl_plot <- renderPlot({
    counts <- head(countsTable(), input$leaderCount)[, 1:4] # only visualize top 20 users
    counts$user <- factor(counts$user, levels=counts$user)
    counts.melt <- melt(counts, id.vars=c("user"))
    counts.melt$value <- as.numeric(counts.melt$value)
    
    # plot (Cleveland dot plot)
    p <- ggplot(counts.melt, aes(x=user, y=value, color=variable)) + geom_point() + 
      coord_flip() + ggtitle("Counts of tweets, retweets, and messages") + 
      xlab("Users") + ylab("Counts")
    print(p)
  })
  
  ## tempo_ppl
#   output$tempo_ppl <- renderPlot({
#     if(input$user != "") {
#       df.sub <- subset(df, from_user==input$user)
#       p <- ggplot(df.sub) + geom_point(aes(x=created_at,y=from_user)) +
#         theme(axis.title.y=element_blank()) + xlab("time")
#       print(p)
#     } else {
#       p <- ggplot(df) + geom_point(aes(x=created_at,y=from_user), position=position_jitter(width=1,height=.5)) +
#         theme(axis.text.y=element_blank()) + xlab("time") + ylab("users")
#       print(p)
#     }
#   })
  
  ## SNA
  #   # construct an SNA igraph (conductor)
  #   snaGraph <- reactive({
  #     source("social_analysis.R")
  #     
  #     # create data frame
  #     rt.df <- CreateSNADataFrame(df, from="from_user", to="retweet_from", linkNames="rt")
  #     rp.df <- CreateSNADataFrame(df, from="from_user", to="reply_to", linkNames="rp")
  #     
  #     sna.df <- rbind(rt.df, rp.df)
  #     
  #     # begin social network analysis plotting
  #     EnsurePackage("igraph")
  #     
  #     # create graph data frame (igraph)
  #     graph.data.frame(sna.df, directed=TRUE)
  #   })
  #   
  #   # get SNA matrix (conductor)
  #   snaMatrix <- reactive({
  #     EnsurePackage("Matrix")
  #     EnsurePackage("SparseM")
  #     
  #     g <- snaGraph()
  #     # plot with sna get adjacency matrix
  #     mat <- get.adjacency(g)
  #     # convert to csr matrix provided by SparseM ref:
  #     # http://cos.name/cn/topic/108758
  #     as.matrix.csr(mat, ncol=ncol(mat))
  #   })
  
  ## sna_plot
  output$sna_plot <- renderPlot({
    EnsurePackage("sna")
    # plot with sna
    # TODO: distinguish colors by link types
    gplot(mat.csr)
  })
  
  ## sna table
  output$sna_stats <- renderTable({
    EnsurePackage("sna")
    measures <- c("density", "reciprocity", "centralization")
    numbers <-c(gden(mat.csr), 
                as.numeric(grecip(mat.csr)), 
                centralization(mat.csr, sna::degree))
    data.frame(measures, values=round(numbers, 4))
  })
  
  #   # detect communities of social network
  #   detectCommunity <- reactive({
  #     g <- snaGraph()
  #     walktrap.community(g, steps=1000, modularity=TRUE)
  #   })
  
  ## cliques
#   output$community_text <- renderText({
#     EnsurePackage("igraph")
#     
#     #     g.wc <- detectCommunity()
#     paste(length(g.wc), "communities was detected using the", g.wc$algorithm, 
#           "algorithm. The measure of mudularity is", round(max(g.wc$modularity), 2), 
#           ". The largest community has", max(sizes(g.wc)), "members.")
#   })
#   #   output$community <- renderPlot({
#   #     g <- snaGraph()
#   #     g.wc <- walktrap.community(g, steps=1000, modularity=TRUE)
#   #     plot(as.dendrogram(g.wc))
#   #   })
#   # membership in the user's clique
#   output$clique <- renderTable({
#     #     g.wc <- detectCommunity()
#     membership <- g.wc$membership[g.wc$names==input$user2]
#     clique <- data.frame(users=g.wc$names[g.wc$membership==membership])
#     if(nrow(clique) > 0) clique else NULL
#   })
  
  
  #   
  #   ## who should you talk to?
  #   output$talkto <- renderTable({
  #     NULL
  #   })
  
  ## download csv
  output$downloadData <- downloadHandler(
    filename = function() { "data/aera13.csv" },
    content = function(file) {
      write.csv(df, file)
    }
  )
  
})
