## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(dplyr)
library(reshape2)
library(googleVis)
library(xts)
library(dygraphs)

mydata = fread("427915.csv", col.names = c("userID", "userNo", "name", "email", "ip","uniqueID", "started", "ended",
                                           "satisfactionLevel", "improvements", "task", "taskLevelOfDifficulty", "taskLevelOfDifficultyReason"));mydata


mydata <- mydata[-c(1,2), ] ##delete first two rows

mydata <- data_frame(userID = mydata$userID, userNo= mydata$userNo, ended =mydata$ended, satisfactionLevel = mydata$satisfactionLevel, improvements = mydata$improvements, task = mydata$task,
                     taskLevelOfDifficulty = mydata$taskLevelOfDifficulty, taskLevelOfDifficultyReason = mydata$taskLevelOfDifficultyReason)



mydata <- mydata %>% 
  mutate(ended = as.Date(ended, format = "%d/%m/%Y"))

mydataWithWeeksAndWeights<- data_frame(ended = mydata$ended, week = format(mydata$ended, format = "%W"), satisfactionLevel = mydata$satisfactionLevel) %>%
  mutate(weight = case_when(
    satisfactionLevel == "Very dissatisfied" ~ 0,
    satisfactionLevel == "Very satisfied" ~ 1,
    satisfactionLevel == "Satisfied" ~ 0.75,
    satisfactionLevel == "Neither satisfied nor dissatisfied" ~ 0.5,
    satisfactionLevel == "Dissatisfied" ~ 0.25)) %>% 
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
  
  filteredDataWithWeeks <- reactive ({
    req(input$dateRange)
    mydataWithWeeksAndWeights[mydataWithWeeksAndWeights$ended >= input$dateRange[1] & mydataWithWeeksAndWeights$ended <= input$dateRange[2],]
    
  })
  
  
  filteredDataWithWeeksAndWeights2 <- reactive({
    req(input$dateRange)
    mydataWithWeeksAndWeights2 <- mydataWithWeeksAndWeights[mydataWithWeeksAndWeights$ended >= input$dateRange[1] & mydataWithWeeksAndWeights$ended <= input$dateRange[2],]
    pivotTable <- mydataWithWeeksAndWeights2 %>% group_by(year_week = format(as.Date(ended, "%d/%m/%Y"), "%Y-%W"), weight) %>% count(satisfactionLevel)
    pivotTable2 <- pivotTable %>% mutate(multiply = weight * n) %>% group_by(year_week) %>% summarize(denominator = sum(n), nominator = sum(multiply)) %>% mutate(satisfaction = nominator / denominator)
    
  })
  

  output$boxPlot <- renderPlot(ggplot(filteredData(), aes(x = reorder(satisfactionLevel, satisfactionLevel, function(x) - length(x)))) +
                                 geom_bar(fill = "steelBlue") + xlab("Satisfaction Level") + 
                                    ylab("Count") + ggtitle("Satisfaction level") + theme_minimal()) 
  
  output$boxPlot2 <- renderPlot(ggplot(filteredData(), aes(x = reorder(task, task, function(x) - length(x)), fill = taskLevelOfDifficulty)) + geom_bar() + 
                                         xlab("Task") + ylab("Count") + ggtitle("Task difficulty") + labs(fill = "Task level of difficulty") +
                                        theme_minimal() + theme(axis.text.x = element_text(angle = 15, hjust = 1)))
  
  output$userSatisfactionLineGraph <- renderPlot(ggplot(filteredDataWithWeeksAndWeights2(), aes(year_week, satisfaction, group=1)) + geom_line() + theme_minimal() +
                                                   xlab("Week of the year") + ylab("Weekly satisfaction level") + ggtitle("User satisfaction") + 
                                                 theme(axis.text.x = element_text(angle = 90, hjust = 1)))
  
  output$numberOfResponses <- renderInfoBox({
    infoBox("Responses for specified period", nrow(filteredData()), icon = icon("user"), fill = TRUE)
  })
  
  output$another2 <- renderInfoBox({
    infoBox("Total responses", nrow(mydata), icon = icon("users"), fill = TRUE)
  })
  
  output$another3 <- renderInfoBox({
    infoBox("Weekly satisfaction rate", tail(filteredDataWithWeeksAndWeights2()[,c(4)], n=1), icon = icon("hashtag "), fill = TRUE)
  })
}


shinyApp(ui, server)