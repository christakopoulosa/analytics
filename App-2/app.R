## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(dplyr)
library(reshape2)
library(googleVis)

mydata = fread("427915.csv", col.names = c("userID", "userNo", "name", "email", "ip","uniqueID", "started", "ended",
                                           "satisfactionLevel", "improvements", "task", "taskLevelOfDifficulty", "taskLevelOfDifficultyReason"));mydata


mydata <- mydata[-c(1,2), ] ##delete first two rows

mydata <- data_frame(userID = mydata$userID, userNo= mydata$userNo, ended =mydata$ended, satisfactionLevel = mydata$satisfactionLevel, improvements = mydata$improvements, task = mydata$task,
                     taskLevelOfDifficulty = mydata$taskLevelOfDifficulty, mydata$taskLevelOfDifficultyReason)

mydata <- mydata %>% 
  mutate(ended = as.Date(ended, format = "%d/%m/%Y"))



ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Smart Survey Insights"), ##header title
  
  dashboardSidebar(    ##sidebar
    dateRangeInput("dateRange", "Please choose date", "2017-01-01", NULL),
    checkboxInput("comparePreviousPeriod", "Compare to previous period")
  ),
  
  dashboardBody(  ##main body
    fluidRow(
      box(width = 6, plotOutput("boxPlot")),
      box(width = 6, plotOutput("boxPlot2"))
    ),
    
    fluidRow(
      box(width = 6, plotOutput("userSatisfactionLineGraph")),
      infoBoxOutput("numberOfResponses"),
      infoBoxOutput("another2"),
      infoBoxOutput("another3")      
      
      )
    )
  )





server <- function(input, output) { 
  
  filteredData <- reactive({
    req(input$dateRange)
    mydata[mydata$ended >= input$dateRange[1] & mydata$ended <= input$dateRange[2],]
  })
  

  output$boxPlot <- renderPlot(ggplot(filteredData(), aes(x=reorder(satisfactionLevel,satisfactionLevel,function(x)-length(x)))) +
                                 geom_bar(fill = "steelBlue") + xlab("Satisfaction Level") + 
                                    ylab("Count") + ggtitle("Count of satisfaction level") + theme_minimal()) 
  
  output$boxPlot2 <- renderPlot(ggplot(filteredData(), aes(task, fill = taskLevelOfDifficulty)) + geom_bar() + 
                                         xlab("Task") + ylab("Count") + ggtitle("Level of difficulty of task performed") + theme_minimal())
  
  output$userSatisfactionLineGraph <- renderPlot(ggplot(filteredData(), aes(ended, satisfactionLevel)) + geom_line())
  
  output$numberOfResponses <- renderInfoBox({
    infoBox("Responses", nrow(filteredData()), icon = icon("hand-peace-o"), fill = TRUE)
  })
  
  output$another2 <- renderInfoBox({
    infoBox("XYZ", nrow(filteredData()), icon = icon("bath"), fill = TRUE)
  })
  
  output$another3 <- renderInfoBox({
    infoBox("XYZ", nrow(filteredData()), icon = icon("battery-quarter"), fill = TRUE)
  })
}


shinyApp(ui, server)