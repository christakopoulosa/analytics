##Mail by 

##Install googleAnalyticsR library
library(googleAnalyticsR)

## authenticate, or use the RStudio Addin "Google API Auth" with analytics scopes set
ga_auth()

## get your accounts
account_list <- google_analytics_account_list()


## account_list will have a column called "viewId"
account_list$viewId

## View account_list and pick the viewId you want to extract data from
ga_id <- account_list$viewId[[6]]

## simple query to derive number of sessions, number of users and pages viewed per session - change it to weekly?
monthlydataSessions <- google_analytics_4(ga_id, 
                     date_range = c("2017-01-01", "2017-10-31"), 
                     metrics = "sessions", dimensions = "yearMonth")

monthlydataUsers <- google_analytics_4(ga_id, 
                                  date_range = c("2017-01-01", "2017-10-31"), 
                                  metrics = "users", dimensions = "yearMonth")


monthlydataPagesViewsPerSession <- google_analytics_4(ga_id, 
                                       date_range = c("2017-01-01", "2017-10-31"), 
                                       metrics = "pageviewsPerSession", dimensions = "yearMonth")

speed <- google_analytics_4(ga_id, 
                               date_range = c("2017-01-01", "2017-10-31"), 
                               metrics = "avgPageLoadTime", dimensions = "yearWeek") ##average page load time for every week and then bind the data to one file with - what kind of week is that

##Create a dataframe with the data monthlydataSessions / monthlydataUsers / monthlydataPagesViewsPerSession
monthlydataSessions <- data_frame(yearMonth = monthlydataSessions$yearMonth, sessions = monthlydataSessions$sessions)
monthlydataUsers <- data_frame(yearMonth = monthlydataUsers$yearMonth, users = monthlydataUsers$users)
monthlydataPagesViewsPerSession <- data_frame(yearMonth = monthlydataPagesViewsPerSession$yearMonth, pageviewsPerSession = monthlydataPagesViewsPerSession$pageviewsPerSession )

##library(dplyer)
library(dplyr)
monthlydata <- monthlydataSessions %>% inner_join(monthlydataUsers) %>%
  inner_join(monthlydataPagesViewsPerSession)

##How to find number of logged in users
##needs to be identified

##Add survey data
library(data.table)
mydata = fread("417016.csv", col.names = c("userID", "userNo", "name", "email", "ip","uniqueID", "started", "ended",
                                           "satisfactionLevel", "improvements", "task", "taskLevelOfDifficulty" 
                                           , "taskLevelOfDifficultyReason"));mydata

##Break text data to tokens (one-token-per-document-per-row)
library(tidytext)
satisfaction <- data_frame(date = mydata$ended, satisfaction = mydata$satisfactionLevel);satisfaction
licensingTask <- data_frame(task = mydata$task, difficulty = mydata$taskLevelOfDifficulty)

##Delte forst two rows
satisfaction <- satisfaction[-c(1,2), ]
licensingTask <- licensingTask[-c(1,2), ]

##Extract data
write.csv(monthlydata, file = "gaSessionsUsersPagesPerSession.csv", row.names = FALSE) ##give descriptive names for columns
write.csv(satisfaction, file = "monthlySatisfactionRates.csv", row.names = FALSE)
write.csv(licensingTask, file = "licensingTask.csv", row.names = FALSE)


##Plot data - reference point
library(ggplot2)
ggplot(data=monthlydata$yearMonth, aes(x = yearMonth, y = sessions,  group = 1)) +  ##where $yearMonth is located you can change it with  $sessions or $users
  geom_line()
