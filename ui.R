library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("#AERA13"),
  
  sidebarPanel(
    
    wellPanel(
      h3("About"),
      HTML("Twitter hashtag analytics for <b><a href='https://twitter.com/search?q=%23AERA13&src=typd' target='_blank'>#AERA13</a></b><br/>"),
      helpText("To explore this dataset, choose analytics below."),
      br(),
      HTML("Developed by: <a href='http://bodongchen.com/' target='_blank'>Bodong Chen</a> (<a href='https://twitter.com/bodongchen' target='_blank'>@bodongchen</a>)")
      ),
    
    wellPanel(
      h3("Choose Analytics"),
      selectInput(inputId = "analytics",
                  label = "",
                  choices = c("Summary" = "summary",
                              "Tweets" = "tweets",
                              "People" = "people")
      ),
      
      conditionalPanel(condition = "input.analytics == 'tweets'",
                       h4("Choose Analysis"),
                       radioButtons("rb_tweets", "",
                                    list(#"Timeline" = "timeline",
                                         "Top URLS" = "urls",
                                         "Top Hashtags" = "hashtags",
                                         "Sentiments" = "sentiments",
                                         "Filter" = "ngram"
 #                                       "Wordcloud" = "wordcloud", 
 #                                       "Topic Modeling" = "topics", 
 #                                       "Semantic Space Visualization" = "semantic"
                                    )),
                       conditionalPanel(condition = "input.rb_tweets == 'urls' || input.rb_tweets == 'hashtags'",
#                                         sliderInput("top_num", "", min=0, max=50, value=20),
                                        uiOutput("numControls")),
                       conditionalPanel(condition = "input.rb_tweets == 'sentiments'",
                                        sliderInput("sentiments_num", "", min=0, max=50, value=10)),
                       conditionalPanel(condition = "input.rb_tweets == 'ngram'",
                                        textInput(inputId = "ngram_query", label = "Tweet"),
                                        textInput(inputId = "user_query", label = "Tweeter")
                       )
      ),
      
      conditionalPanel(condition = "input.analytics == 'people'",
                       h4("Choose Analysis"),
                       radioButtons("rb_people", "",
                                    list("Top Tweeters" = "counts",
#                                          "Timeline by Person" = "tempo_ppl",
                                         "Social Network" = "sna"
#                                          "Sub-communities" = "cliques"
                                         #                                       "Who should talk to whom?" = "towhom"
                                    )),
                       conditionalPanel(condition = "input.rb_people == 'counts'",
                                        uiOutput("leaderSlider"))
#                        conditionalPanel(condition = "input.rb_people == 'tempo_ppl'",
#                                         textInput("user", "type a username to track:", "")
#                        ),
#                        conditionalPanel(condition = "input.rb_people == 'cliques'",
#                                         textInput("user2", "type a username to find his/her clique:", ""))
      )
    ),
    
    downloadButton('downloadData', 'Download as CSV')
    
    # submitButton("Update View")
  ),
  
  mainPanel(
    
    conditionalPanel(condition = "input.analytics == 'summary'",
                     h3("Summary"),
                     helpText("This tweet dataset is from the 2013 AERA Annual Meeting."),
                     textOutput("summary"),
                     br(),
                     h4("Tweet timeline"),
                     plotOutput(outputId = "tweets_timeline")
#                      plotOutput(outputId = "wordcloud")
#                      br(),
#                      helpText("Note: Because of the volume of tweets, the application may take around 5 seconds to load."),
#                      helpText("After the summary is diaplayed, please choose a type of analytics on the sidebar to begin.")
    ),
    
    conditionalPanel(condition = "input.analytics == 'tweets'",
#                      h3("Tweets"),
#                      conditionalPanel(condition = "input.rb_tweets == 'timeline'",
#                                       h4("Timeline"),
#                                       plotOutput(outputId = "tweets_timeline")
#                      ),
                     conditionalPanel(condition = "input.rb_tweets == 'urls'",
                                      h4("Top URLs"),
                                      htmlOutput(outputId = "urls_table")
                     ),
                     conditionalPanel(condition = "input.rb_tweets == 'hashtags'",
                                      h4("Top Hashtags"),
                                      htmlOutput(outputId = "hashtags_table")
                     ),
                     #                      conditionalPanel(condition = "input.wordcloud == true",
                     #                                       plotOutput(outputId = "wordcloud")
                     #                      ),
                     #                      conditionalPanel(condition = "input.topics == true",
                     #                                       tableOutput(outputId = "topics")
                     #                      ),
                     #                      conditionalPanel(condition = "input.mds == true",
                     #                                       plotOutput(outputId = "mds")
                     #                      ),
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
                     conditionalPanel(condition = "input.rb_tweets == 'ngram' && (input.ngram_query != '' || input.user_query != '')",
                                      h4("Tweets"),
                                      htmlOutput(outputId = "search_table")
                     )
    ),
    
    conditionalPanel(condition = "input.analytics == 'people'",
#                      h3("People"),
                     conditionalPanel(condition = "input.rb_people == 'counts'",
                                      h4("Top Tweeters"),
                                      tabsetPanel(
                                        tabPanel("Table", tableOutput(outputId = "counts_ppl")),
                                        tabPanel("Plot", plotOutput(outputId = "counts_ppl_plot"))
                                      )
                     ),
#                      conditionalPanel(condition = "input.rb_people == 'tempo_ppl'",
#                                       h4("Patterns of individual users:"),
#                                       plotOutput(outputId = "tempo_ppl")
#                      ),
                     conditionalPanel(condition = "input.rb_people == 'sna'",
                                      h4("Social Network Analysis"),
                                      tabsetPanel(
                                        tabPanel("Network visualization", 
                                                 plotOutput(outputId = "sna_plot"), 
                                                 HTML("For an interactive version, check out <a href='http://hawksey.info/tagsexplorer/?key=tWLjcg2SCtalHPwBhNP7xuQ&sheet=oaw' target='_blank'>TAGSExplorer</a> (Courtesy of <a href='http://twitter.com/mhawksey' target='_blank'>Martin Hawksey</a>)")
                                                 ),
                                        tabPanel("Network measures", 
                                                 tableOutput(outputId = "sna_stats")
                                                 )
                                      )
                     )
#                      conditionalPanel(condition = "input.rb_people == 'cliques'",
#                                       h4("Community detection:"),
#                                       textOutput("community_text"),
#                                       # plotOutput(outputId = "community"),
#                                       br(),
#                                       h4("Users in the same community:"),
#                                       tableOutput(outputId = "clique")
#                      )
    )
  )
))