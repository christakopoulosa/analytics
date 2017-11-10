##adding libraries
library(shiny)
library(googleAnalyticsR)
library(flexdashboard)
library(xts)


##make a shiny dashbordt that pull in google analytics data
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
      h2("plain text"), #need to add of google analytics code 
      h3("i need to add graphs here and there"),
      h6("another instance")
      #strong("this is a title in bold") incorrect html code for the page
      #mark's edmonton page
    )
  )
)

##need to check othe

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
 
  
}

shinyApp(ui = ui, server = server)
