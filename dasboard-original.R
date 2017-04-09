library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)

# To create and save omaha map data from goole
# omaps <-  get_map(location = 'Omaha', maptype = 'roadmap', zoom = 11, color='bw')
# save(omaps, file = "omaps.RData") # get_map(location = 'Omaha', source = 'stamen', maptype = 'toner')
# Once saved, we don't need to connect google, we can just load

load("omaps.RData") #obtained using get_map()
crimes <- read.csv("omaha-crimes.csv")

ui <- dashboardPage(
  dashboardHeader(title = "My Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("My control Center", tabName = "control", 
               icon = icon("dashboard")),
      menuItem("My city crime", tabName = "oCrime", 
               icon = icon("th"))
    )    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "control",
              fluidRow(
                box(plotOutput("myPlot", height = 250)),
                box(
                  title = "Controls",
                  sliderInput("slider", 
                              "Number of observations:", 1, 100, 50)
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "oCrime",
              h2("Omaha crime map goes here"),
              fluidRow(
                box(plotOutput("myMap"), height = 450, width=400),
                box(title = "Please select a crime",
                    selectizeInput("crimeType", label="Crime Type",
                                   choices = crimes$type,
                                   selected = crimes$type[1:3],
                                   multiple=TRUE)
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$myPlot <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$myMap <- renderPlot({
    crimes_sub <- subset(crimes, crimes$type %in% input$crimeType)
    ggmap(omaps) +
      geom_point(size=5, alpha = 1/2, aes(lon,lat, color=type), 
                 data = crimes_sub)
  })
  
}

shinyApp(ui, server)
