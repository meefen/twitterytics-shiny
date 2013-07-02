Twitterytics-Shiny
========================

## Steps to deploy a Twitterytics-Shiny app

1. Register an account at [RStudio's Shiny server](https://rstudio.wufoo.com/forms/shiny-server-beta-program/) or [install a Shiny server on your own](http://www.rstudio.com/shiny/)
2. Login to your Shiny server, and clone this repo to a proper folder following [Joe Cheng's guide](https://groups.google.com/forum/#!topic/shiny-discuss/ACgJu_c_Yks)
3. Setup a Google Spreadsheet archive for your tweets using [TAGS-5](http://mashe.hawksey.info/2013/02/twitter-archive-tagsv5/), and publish this archive to make it accessible for Shiny
4. On your Shiny server, edit setting.txt
  - hashtag
  - Google Spreadsheet key
  - timezone
5. Run preprocessing.R to process data, or run repeat_pre.R if you wish to automatically update your data every hour.
6. Ready to go, check http://glimmer.rstudio.com/your_username/folder_name
