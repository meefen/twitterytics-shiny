library(shiny)

hashtag <- read.table("settings.txt", header=FALSE)[1, 1]

shinyUI(pageWithSidebar(
  
  headerPanel(paste0("#", hashtag)),
  
  sidebarPanel(
    
    wellPanel(
      h3("About"),
      HTML(paste0("Twitter hashtag analytics for 
                  <b><a href='https://twitter.com/search?q=%23", 
                  hashtag, "&src=typd' target='_blank'>#", 
                  hashtag, "</a></b>.")),
      br(),
      helpText("To explore this dataset, choose analytics below."),
      br(),
      HTML(paste(textOutput("hits"), 
                 "by: <a href='http://bodongchen.com/' target='_blank'>Bodong Chen</a>"))
      ),
    
    wellPanel(
      h3("Choose Analytics"),
      selectInput(inputId = "analytics",
                  label = "",
                  choices = c("Summary" = "summary",
                              "Tweets" = "tweets",
                              "Tweeters" = "people")
      ),
      
      conditionalPanel(condition = "input.analytics == 'tweets'",
                       h4("Choose Analysis"),
                       radioButtons("rb_tweets", "",
                                    list("Filter" = "ngram",
                                         "Top URLs" = "urls",
                                         "Top Hashtags" = "hashtags",
                                         "Sentiments" = "sentiments"
                                    )),
                       conditionalPanel(condition = "input.rb_tweets == 'urls' 
                                        || input.rb_tweets == 'hashtags'",
                                        uiOutput("numControls")),
                       conditionalPanel(condition = "input.rb_tweets == 'sentiments'",
                                        sliderInput("sentiments_num", "", min=0, max=50, value=10)),
                       conditionalPanel(condition = "input.rb_tweets == 'ngram'",
                                        br(),
                                        textInput(inputId = "ngram_query", label = "Filter by tweet:"),
                                        textInput(inputId = "user_query", label = "Filter by tweeter:"),
                                        helpText("Note: You can filter by tweet content and/or tweeters.")
                       )
      ),
      
      conditionalPanel(condition = "input.analytics == 'people'",
                       h4("Choose Analysis"),
                       radioButtons("rb_people", "",
                                    list("Top Tweeters" = "counts",
                                         "Social Network" = "sna"
                                    )),
                       conditionalPanel(condition = "input.rb_people == 'counts'",
                                        uiOutput("leaderSlider"))
      )
    ),
    
    downloadButton('downloadData', 'Download archive as CSV')
  ),
  
  mainPanel(
    
#     HTML("<script type='text/javascript', src='www/googleanalytics.js'></script>"),
    
    conditionalPanel(condition = "input.analytics == 'summary'",
                     h3("Summary"),
#                      helpText("Given the sheer volume of tweets, the page might take a few seconds to load."),
                     textOutput("summary"),
                     br(),
                     h4("Timeline"),
                     plotOutput(outputId = "tweets_timeline")
    ),
    
    conditionalPanel(condition = "input.analytics == 'tweets'",
                     conditionalPanel(condition = "input.rb_tweets == 'urls'",
                                      h4("Top URLs"),
                                      htmlOutput(outputId = "urls_table")
#                                       helpText("Note: URLs might not be displayed properly at this moment, because the LongURL api is having trouble unshorting t.co urls.")
#                                       helpText("Note: URLs without a proper (unshortened) title are usually PDFs.")
                     ),
                     conditionalPanel(condition = "input.rb_tweets == 'hashtags'",
                                      h4("Top Hashtags"),
                                      htmlOutput(outputId = "hashtags_table")
                     ),
                     conditionalPanel(condition = "input.rb_tweets == 'sentiments'",
                                      h4("Sentiments of tweets"),
                                      tabsetPanel(
                                        tabPanel("Distribution", plotOutput(outputId = "sentiments")), 
                                        tabPanel("Happiest tweets", htmlOutput(outputId = "happy_tweets")), 
                                        tabPanel("Saddest tweets", htmlOutput(outputId = "sad_tweets"))
                                      )
                     ),
                     conditionalPanel(condition = "input.ngram_query != '' && input.rb_tweets == 'ngram'",
                                      h4("N-gram viewer"),
                                      plotOutput(outputId = "ngram_plot")
                     ),
                     conditionalPanel(condition = "input.rb_tweets == 'ngram'",
                                      h4("Tweets"),
                                      htmlOutput(outputId = "search_table")
                     )
    ),
    
    conditionalPanel(condition = "input.analytics == 'people'",
                     conditionalPanel(condition = "input.rb_people == 'counts'",
                                      h4("Top Tweeters"),
                                      tabsetPanel(
                                        tabPanel("Table", tableOutput(outputId = "counts_ppl")),
                                        tabPanel("Plot", plotOutput(outputId = "counts_ppl_plot"))
                                      )
                     ),
                     conditionalPanel(condition = "input.rb_people == 'sna'",
                                      h4("Social Network Analysis"),
                                      tabsetPanel(
                                        tabPanel("Network visualization", 
                                                 plotOutput(outputId = "sna_plot"), 
                                                 HTML("For an interactive version, check out <b><a href='http://hawksey.info/tagsexplorer/?key=tWLjcg2SCtalHPwBhNP7xuQ&sheet=oaw' target='_blank'>TAGSExplorer</a></b> (Courtesy of <a href='http://twitter.com/mhawksey' target='_blank'>Martin Hawksey</a>)")
                                                 ),
                                        tabPanel("Network measures", 
                                                 tableOutput(outputId = "sna_stats")
                                                 )
                                      )
                     )
    )
  )
))