library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(data.table)
library(scales)
library(choroplethr)
library(choroplethrMaps)
library(plotly)
library(stringr)
library(lazyeval)
library(dplyr)

# To create and save omaha map data from goole
# omaps <-  get_map(location = 'Omaha', maptype = 'roadmap', zoom = 11, color='bw')
# save(omaps, file = "omaps.RData") # get_map(location = 'Omaha', source = 'stamen', maptype = 'toner')
# Once saved, we don't need to connect google, we can just load

load("omaps.RData") #obtained using get_map()
crimes <- read.csv("omaha-crimes.csv")
    
crimeDat <- readRDS("usaCrimeDat.rds")
myCrime <- as.character(unique(crimeDat$Crime))
mdat <- map_data("state")

ui <- dashboardPage(
  dashboardHeader(title = "My Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("My control Center", tabName = "control", 
               icon = icon("dashboard")),
      menuItem("My city crime", tabName = "oCrime", 
               icon = icon("th")),
      menuItem("My state crime", tabName = "sCrime", 
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
                    selectizeInput("crimeType", label = "Crime Type",
                                                choices = crimes$type,
                                                selected = crimes$type[1:3],
                                                multiple = TRUE)
                )
              )
      ),
      
      tabItem(tabName = "sCrime",
              h2("My state crime map goes here"),
              fluidRow(
                box(plotOutput("myMapCrime"), height = 450, width=400),
                column(5,
                       wellPanel(
                         selectInput("selectedCrime", 
                                     label = "Choose a crime to display",
                                     choices = myCrime, 
                                     selected = myCrime[1]),
                         
                         sliderInput("myYears",
                                     "Crime Year",
                                     min = 1969,
                                     max = 2017,
                                     value = 1980),
                         "This is a project created to demonstrate how shiny application 
              can be used for exploratory data analysis. For more information 
              about this project please visit github repository 
              https://github.com/mamajumder/usa-crime"
                       ) 
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
  
  output$myMapCrime <- renderPlot({

    crimes_sub <- subset(crimeDat, crimeDat$Crime %in% input$selectedCrime)
    crimes_sub <- subset(crimeDat, crimeDat$Year <= input$myYears)
    
    combdat <- merge(mdat, crimes_sub, by.x=c('region'), 
                     by.y=c('state'), all.x=TRUE)
    
    odat <- combdat[order(combdat$order),]
    ggplot(odat, aes(x=long, y=lat,group=group)) +
      geom_polygon(aes(fill=rate), colour = alpha("white", 0.2)) + 
      theme_bw() + scale_fill_continuous(low="blue", high="pink") +
      theme(
        legend.position = "none",
        text = element_blank(), 
        line = element_blank())  
  })
}

shinyApp(ui, server)
