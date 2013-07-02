source("utils/utilities.R")

ConstructCorpus <- function(textVec, 
                            toLower = TRUE, 
                            removePunctuations = TRUE, 
                            removeStopwords = TRUE, 
                            removeNumbers = FALSE, 
                            stemming = FALSE,
                            removeTags = FALSE, 
                            removeUsers = FALSE) {
  # Construct text corpus
  
  more.stopwords <- c("via", "rt", "mt", "amp")
  
  EnsurePackage("tm")
  
  # create a object
  corpus <- Corpus(VectorSource(textVec))
  
  if(toLower) corpus <- tm_map(corpus, tolower)
  
  if(removeTags) {
    corpus <- tm_map(corpus, TrimHashtags)
  }
  if(removeUsers) {
    corpus <- tm_map(corpus, TrimUsers)
  }
  
  if(removePunctuations) corpus <- tm_map(corpus, removePunctuation) 
  if(removeNumbers) corpus <- tm_map(corpus, removeNumbers)
  if(removeStopwords) corpus <- tm_map(corpus, function(x) 
    removeWords(x, append(stopwords("english"), more.stopwords)))
  
  if(stemming) {
    EnsurePackage("rJava")
    EnsurePackage("Snowball")
    corpus <- tm_map(corpus, stemDocument, language = "english")
  }
  
  return(corpus)
}

MakeWordCloud <- function(corpus) {
  # Make a word cloud
  #
  # Args:
  #   textVec: a text vector
  #
  # Returns:
  #   A word cloud created from the text vector
  
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
  EnsurePackage("RColorBrewer")
  
  corpus <- tm_map(corpus, function(x) {
    removeWords(x, c("via", "rt", "mt"))
  })
  
  ap.tdm <- TermDocumentMatrix(corpus)
  ap.m <- as.matrix(ap.tdm)
  ap.v <- sort(rowSums(ap.m), decreasing=TRUE)
  ap.d <- data.frame(word = names(ap.v), freq=ap.v)
  table(ap.d$freq)
  pal2 <- brewer.pal(8, "Dark2")
  
  p <- wordcloud(ap.d$word, ap.d$freq, 
            scale=c(8, .2), min.freq = 3, 
            max.words = Inf, random.order = FALSE, 
            rot.per = .15, colors = pal2)
  p
}

TrainLDAModel <- function(td.mat) {
  # Train a LDA model based on a sparse term-document matrix
  # 
  # Args:
  #   td.mat.sp: a sparse term-document matrix
  #
  # Returns:
  #   A LDA model with the optimal number of topics
  
  EnsurePackage("topicmodels") # have to install libgsl0-dev before installing this package on Ubuntu
  EnsurePackage("slam")
  
  # create document term matrix and convert to data frame
  td.mat.sp <- removeSparseTerms(td.mat, sparse=0.99)
#   td.mat.sp.df <- as.data.frame(inspect(td.mat.sp))
  # check how many words are left
#   nrow(td.mat.sp.df)
  
  # transpose document term matrix
  td.mat.sp.t <- t(td.mat.sp)
  # summary(col_sums(td.mat.sp.t)) # check median
  
  # calculate tf-idf values
  term_tfidf <- tapply(td.mat.sp.t$v/row_sums(td.mat.sp.t)[td.mat.sp.t$i], td.mat.sp.t$j,mean) * 
    log2(nDocs(td.mat.sp.t)/col_sums(td.mat.sp.t > 0))
  # summary(term_tfidf) # check median... note value for next line...
  
  # keep only those terms that are slightly less frequent that the median
  td.mat.sp.t.tdif <- td.mat.sp.t[, term_tfidf >= as.numeric(summary(term_tfidf)[3])]
  td.mat.sp.t.tdif <- td.mat.sp.t[row_sums(td.mat.sp.t) > 0, ]
  # summary(col_sums(td.mat.sp.t.tdif)) # have a look
  
  # train a topic model for every number of topics between 2 and 50 (may take long)
  best.model <- lapply(seq(2, 50, by = 1), function(d) LDA(td.mat.sp.t.tdif, d))
  
  # a list of logLiks for each model
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  
  # Find the number of topics which has the highest log likelihood
  best.model.logLik.df <- data.frame(topics=c(2:50), 
                                     LL=as.numeric(as.matrix(best.model.logLik)))
  optimal.num <- best.model.logLik.df$topics[
    which(best.model.logLik.df$LL == max(best.model.logLik.df$LL))]
  
  # plot the distribution of logliklihoods by topic
#   ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
#     xlab("Number of topics") + 
#     ylab("Log likelihood of the model") + 
#     geom_line() + 
#     geom_vline(xintercept=optimal.num, linetype="dotted", colour="red") + 
#     annotate("text", x=optimal.num, y=25, label=paste("num =",optimal.num), hjust=0)
  
  # generate a LDA model with the best number of topics
  LDA(td.mat.sp.t.tdif, optimal.num)
}

ScoreSentiment <- function(sentences, .progress='none') {
  # Score sentiment of sentences
  # Ref: http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
  
  EnsurePackage("plyr")
  EnsurePackage("stringr")
  
  #load sentiment lexicon
  pos.words = c(scan('./data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';'))
  neg.words = c(scan('./data/opinion-lexicon-English/negative-words.txt', what='character', comment.char= ';'))
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores <- laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list <- str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=as.character(sentences))
  return(scores.df)
}

ScoreSentiment2 <- function(sentences){
  # Score sentiment through 'sentiment' package
  
  # To install sentiment, check:
  # https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
  # install 'Rstem' and 'sentiment' from source
  # use command like: sudo R CMD INSTALL sentiment_0.2.tar.gz
  
  EnsurePackage("sentiment")
  
  class_emo <- classify_emotion(sentences, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion <- class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] <- "unknown"
  
  # classify polarity
  class_pol <- classify_polarity(sentences, algorithm="bayes")
  
 # data frame with results
  sent.df <- data.frame(text=sentences, 
                        emotion=emotion, 
                        score=as.numeric(class_pol[, 3]),
                        best_fit=class_pol[, 4],
                        stringsAsFactors=FALSE)
  
  return(sent.df)
}

ScoreSentimentViralHeat <- function(text, key = "QKzubH1Qv6n9ZU4Jw") {
  # Score sentiment with ViralHeat
  # TODO: doesn't work for now; json returns empty
  
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  
  text <- URLencode(text)
  
  # save all the spaces, then get rid of the weird characters that break the API, 
  # then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ")
  text <- str_replace_all(text, "%\\d\\d", "")
  text <- str_replace_all(text, " ", "%20")
  
  if(str_length(text) > 360) text <- substr(text, 0, 359)
  
  data <- getURL(paste("http://www.viralheat.com/api/sentiment/review.json?api_key=", key, 
                       "&text=", text, sep=""))
  
  js <- fromJSON(data, asText=TRUE)
  
  mood <- js$prob
  
  if (js$mood == "negative"){
    mood <- mood * -1
  } else if(js$mood == "positive") {
    # do nothing
  } else {
    mood <- 0
  }
  
  return(mood)
}