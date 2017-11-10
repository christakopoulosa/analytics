##Libraries
library(shiny)
library(googleAnalyticsR)
library(flexdashboard)
library(xts)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Smart Survey Data dashbord"),
  sidebarLayout(
    sidebarPanel("this is the sidebar",
                 dateRangeInput(inputId = "date", label = "Date range", start = "2017/10/21", end = "2017/10/31")
                 ),
    
    mainPanel(
      
      h2("This is a graph for user satiafction"),
      h2("This is a graph of task completions"),
      h2("Test title 1"), #need to add of google analytics code 
      h3("Test title 2")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  }

shinyApp(ui = ui, server = server)
