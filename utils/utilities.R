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

decode_short_url <- function(url, ...) {
  ## Inspired by: http://goo.gl/Q6mi8
  
  # PACKAGES #
  require(RCurl)
  require(XML)
  
  # LOCAL FUNCTIONS #
  decode <- function(u) {
    Sys.sleep(0.5)
    x <- try(getURL(u, header = TRUE, #nobody = TRUE, 
                     followlocation = FALSE, 
                     cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")),
             silent=TRUE)
    if(inherits(x, 'try-error') | length(grep(".*[Ll]ocation: (\\S+).*", x))<1) {
      return(u)
    } else {
      return(gsub('.*[Ll]ocation: (\\S+).*', '\\1', x))
    }
  }
  
  # MAIN #
  gc()
  # return decoded URLs
  urls <- c(url, ...)
  l <- vector(mode = "list", length = length(urls))
  l <- lapply(urls, decode)
  names(l) <- urls
  return(l)
}

UnshortenURL <- function(l) {
  
  EnsurePackage("R.utils")
  require(RCurl)
  EnsurePackage("XML")
  
#   cat(l)
  
  # if blank, return blanks
  if(is.na(l) || l == "") return(c("", "", ""))
  
  # load already parsed data
  urls <- data.frame(short=c(), long=c(), title=c())
  tryCatch(load("data/urls.Rda"), 
           error = function(e) {
             cat("no urls data yet. will create new.")
           },
           warning = function(e) {
             cat("no urls data yet. will create new.")
           })
  
  # check whether the url has been parsed before
  if(l %in% urls$short) {
    return(urls[urls$short==l, ])
  }
  
  # if not
  longurl <- decode_short_url(l)
#   url <- evalWithTimeout(getURL(longurl), 
#                          timeout=1.08, cpu=1.08, onTimeout="warning")
  ## wiredly, http://t.co/cBUmqOOmSB will Gateway timeout,
  ## even evalWithTimeout could not timeout it. wondering
  ## whether it was because of Stanford Vister wifi's restriction
  html <- try(htmlParse(getURL(longurl), encoding = "UTF-8"), silent=TRUE)
  if(inherits(html, 'try-error')) {
    count <- 5
    repeat{
      longurl <- decode_short_url(longurl)
      html <- try(htmlParse(getURL(longurl), encoding = "UTF-8"), silent=TRUE)
      if(count == 0 || !inherits(html, 'try-error')) {
        break
      }
      count <- count-1
    }
  }
  title <- "Unknown (probably a PDF file)"
  if(!inherits(html, 'try-error')) {
    # http://stackoverflow.com/a/13730279/1094038
    title  <- trim(gsub("\n", "", xpathSApply(html,"//title", xmlValue)))
  }
  v <- c(l, as.character(longurl), title)
  urls <- rbind(urls, v)
  names(urls) <- c("short", "long", "title")
  save(urls, file="data/urls.Rda")
  
  return(v)
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