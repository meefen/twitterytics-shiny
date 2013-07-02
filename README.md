Twitterytics-Shiny
========================

## Steps to deploy a Twitterytics-Shiny app

1. Register an account at [RStudio's Shiny server](https://rstudio.wufoo.com/forms/shiny-server-beta-program/) (it usually take a few days for them to setup), or [install a Shiny server on your own](https://github.com/rstudio/shiny-server)
2. Login to your Shiny server, and clone this repo to the ShinnyApps folder following [Joe Cheng's guide](https://groups.google.com/forum/#!topic/shiny-discuss/ACgJu_c_Yks) and rename it to something meaningful (the hashtag is a quite straightforward choice)
3. Setup a Google Spreadsheet archive for your tweets using [TAGS-5](http://mashe.hawksey.info/2013/02/twitter-archive-tagsv5/), and publish this archive to make it accessible for Shiny. Record the Google Spreadsheet key for later setup.
4. On your Shiny server, edit a few settings in setting.txt
  1. hashtag
  2. Google Spreadsheet key
  3. timezone
5. Run preprocessing.R to process data, or run repeat_pre.R if you wish to automatically update your data every hour.
6. Check http://glimmer.rstudio.com/your_username/folder_name, and see whether everything works.
