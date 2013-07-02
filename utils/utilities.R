# Utilities for this package

options(stringsAsFactors=FALSE)

EnsurePackage <- function(x) {
  # EnsurePackage(x) - Installs and loads a package if necessary
  # Args:
  #   x: name of package
  
  x <- as.character(x)
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}

TrimAt <- function(x) {
  # remove @ from text
  
  sub('@', '', x)
}

TrimHead <- function(x) {
  # remove starting @, .@, RT @, MT @, etc.
  
  sub('^(.*)?@', '', x)
}

TrimUsers <- function(x) {
  # remove users, i.e. "@user", in a tweet
  
  str_replace_all(x, '(@[[:alnum:]_]*)', '')
}

TrimHashtags <- function(x) {
  # remove hashtags, i.e. "#tag", in a tweet
  
  str_replace_all(x, '(#[[:alnum:]_]*)', '')
}

TrimUrls <- function(x) {
  # remove urls in a tweet
  
  str_replace_all(x, 'http[^[:blank:]]+', '')
}

TrimHTMLBreaks <- function(x) {
  # remove HTML breaks, such as \t, \n, \r
  
  x <- sub("\n", "", x)
  x <- sub("\t", "", x)
  sub("\r", "", x)
}

TrimOddChar <- function(x) {
  # remove odd charactors
  iconv(x, to = 'UTF-8')
}

UnshortenURL <- function(l) {
  # unshorten urls with unshort.me API
  
  cat(l)
  if(is.na(l) || l == "") return("")
  
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  
  tryCatch({
    json <- getURL(paste0("http://api.unshort.me/?r=", l, "&t=json"), verbose = TRUE)
    as.character(fromJSON(json))[1]
  }, error = function(e) {
    ""
  })
}

UnshortenURL2 <- function(l) {
  # unshorten urls with longurl.org API
  # this method is much faster than the other one
  
  cat(l)
  
  # if blank, return blanks
  if(is.na(l) || l == "") return(c("", ""))
  
  # load already parsed data
  urls <- data.frame(short=c(), long=c(), title=c())
  tryCatch(load("data/urls.Rda"), 
           error = function(e) {
             cat("no urls data yet.")
           },
           warning = function(e) {
             cat("no urls data yet.")
           })
  
  # check whether the url has been parsed before
  if(l %in% urls$short) {
    return(urls[urls$short==l, 2:3])
  }
  
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  
  tryCatch({
    json <- getURL(paste0("http://api.longurl.org/v2/expand?format=json&title=1&url=", 
                          URLencode(l, TRUE)), verbose = TRUE)
    v <- as.vector(fromJSON(json))
    urls <- rbind(urls, c(l, v[1], v[2]))
    names(urls) <- c("short", "long", "title")
    save(urls, file="data/urls.Rda")
    return(v)
  }, error = function(e) {
    cat("error")
    return(c("", ""))
  })
}

GetTitleOfURL <- function(url) {
  # get title of a link, with RCurl package
  
  if(is.na(url) || url == "") return
  
  EnsurePackage("RCurl")
  html <- getURLContent(url)[[1]]
  title.idx <- regexec("<title>(.*)</title>", html)
  if(length(title[[1]]) == 1) title.idx <- regexec("<h1>(.*)</h1>", html)
  title <- substr(html, title.idx[[1]][2], title.idx[[1]][2] + attr(title.idx[[1]],"match.length")[2] - 1)
  TrimHTMLBreaks(title)
}

CosineSimilarity <- function(va, vb) {
  # Computer cosine similarity between two numeric vectors of the same length
  
  crossprod(va, vb) / sqrt(crossprod(va) * crossprod(vb))
}

## functions for building html tables
# a regular cell
cell_html <- function(cell) {
  paste0('<td>', cell, '</td>')
}

# a link cell (content is a link)
linkCell_html <- function(link, title=NA) {
  if(is.na(title)) title <- link
  paste0('<td><a href="', link, '" target="_blank">', 
         title, '</a></td>')
}

# a hashtag cell, with link to the hashtag
hashCell_html <- function(cell) {
  paste0('<td><a href="https://twitter.com/search?q=%23', 
         substr(cell, 2, nchar(cell)), 
         '" target="_blank">', cell, '</a></td>')
}

# a user cell, with link to the user
userCell_html <- function(cell) {
  paste0('<td><a href="https://twitter.com/', 
         cell, '" target="_blank">', cell, '</a></td>')
}

# a row for link table
link_row_html <- function(row) {
  collapse_cells <- paste0(linkCell_html(row[1]), 
                           cell_html(row[2]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

link_row_html2 <- function(row) {
  link <- row[3]
  if(is.na(link) || link == "") link <- row[1]
  title <- row[4]
  if(is.na(title) || title == "") title <- row[1]
  collapse_cells <- paste0(linkCell_html(link, title), 
                           cell_html(row[2]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

# a row for hashtag table
hashtag_row_html <- function(row) {
  collapse_cells <- paste0(hashCell_html(row[1]), 
                           cell_html(row[2]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

# a row for sentiment tweet
tweet_row_html <- function(row) {
  collapse_cells <- paste0(cell_html(row[1]), 
                           cell_html(row[2]), 
                           userCell_html(row[3]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

# a row for search result tweet
search_row_html <- function(row) {
  hit <- as.numeric(row[4])
  if(hit == 0.01) { ## only filter by user
    collapse_cells <- paste0(cell_html(as.character(row[1])), 
                             userCell_html(row[3]), 
                             cell_html(row[2]), 
                             collapse='')
    paste0('<tr>', collapse_cells, '</tr>')
  } else {
    collapse_cells <- paste0(cell_html(row[4]), 
                             cell_html(as.character(row[1])), 
                             userCell_html(row[3]), 
                             cell_html(row[2]), 
                             collapse='')
    paste0('<tr>', collapse_cells, '</tr>')
  }
}

# a row for leader tweeter
leader_row_html <- function(row) {
  collapse_cells <- paste0(userCell_html(row[1]), 
                           cell_html(row[2]), 
                           cell_html(row[3]), 
                           cell_html(row[4]), 
                           cell_html(row[5]), 
                           collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}