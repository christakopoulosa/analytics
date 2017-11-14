##Libraries
library(shiny)
library(googleAnalyticsR)
library(flexdashboard)
library(xts)
library(dplyr)
library(ggplot2)

##Add survey data
library(data.table)
mydata = fread("427915.csv", col.names = c("userID", "userNo", "name", "email", "ip","uniqueID", "started", "ended",
                                           "satisfactionLevel", "improvements", "task", "taskLevelOfDifficulty" 
                                           , "taskLevelOfDifficultyReason"));mydata


mydata <- mydata[-c(1,2), ] ##delete first two rows

mydata <- data_frame(userID = mydata$userID, userNo= mydata$userNo, ended =mydata$ended, satisfactionLevel = mydata$satisfactionLevel, improvements = mydata$improvements, task = mydata$task,
                     taskLevelOfDifficulty = mydata$taskLevelOfDifficulty, mydata$taskLevelOfDifficultyReason) ##make data set to a data_frame for easy plotting with ggplot2

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











# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("VOL Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      "this is the sidebar",
      dateRangeInput(inputId = "date", label = "Choose date range", start = "2017/10/21", end = "2017/10/31"),
      
      # Add a checkbox to compare to previous period
      checkboxInput("previousPeriod", label = "Compare to previous period")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h2("User satisfaction", align = "center"),
      h2("Task Performed", align = "center"),
      h2("Popular Journeys", align = "center"), #need to add of google analytics code 
      textOutput("selected_var"),
      plotOutput("Satisfaction_level"),
      plotOutput("google_analytics"),
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
   
    server <- function(input, output) {
      output$selected_var <- renderText({
        "I don't know what I am doing"
      })
      
      output$google_analytics <- renderPlot({
        ggplot(data = monthlydataSessions, aes(x=yearMonth, y=sessions, group=1)) + geom_line()
        
      })
      
      output$google_analytics <- renderPlot({
        ggplot(data = monthlydataSessions, aes(x=yearMonth, y=sessions, group=1)) + geom_bar()
        
      })
      
      
      output$google_analytics <- renderPlot({
        ggplot(mydata, aes(x=taskLevelOfDifficulty, y=sessions, group=1)) + geom_bar()
        
      })
      
    }
  
shinyApp(ui = ui, server = server)

