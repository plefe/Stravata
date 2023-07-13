library(readxl)
library(tidyverse)
library(janitor)
library(highcharter)
#test
stravata_dirty <- read.csv("strava.csv")

stravata <- stravata_dirty %>%
  #mutate()
  select(-c(Filename, Athlete.Weight, Commute.1, From.Upload, Activity.ID, Activity.Description,
            Activity.Private.Note, Perceived.Exertion, Media, Prefer.Perceived.Exertion,
            Perceived.Relative.Effort, starts_with("X.span"))) %>%
  .[ , colSums(is.na(.)) != nrow(.)] %>% 
  mutate(Activity.Date = as.POSIXct(Activity.Date, format = "%b %d, %Y, %I:%M:%S %p")) %>%
  mutate(Elapsed.Time = round(Elapsed.Time/60, 2)) %>% 
  mutate(Distance = round(Distance*0.621371, 2)) %>% 
  clean_names('upper_camel')

str(stravata)

hchart(stravata, "scatter", hcaes(x = ElapsedTime, y = MaxHeartRate)) %>%
  hc_title(text = "Activity time vs Max heart rate") %>%
  hc_xAxis(title = list(text = "Elapsed Time (seconds)")) %>%
  hc_yAxis(title = list(text = "Max Heart Rate"))

#distance
hchart(stravata, "column", hcaes(x = Distance)) %>%
  hc_title(text = "Distribution of Activity Distance") %>%
  hc_xAxis(title = list(text = "Distance"))

#activity type
stravata %>%
  group_by(ActivityType) %>%
  summarise(count = n()) %>%
  hchart("column", hcaes(x = ActivityType, y = count)) %>%
  hc_title(text = "Frequency of Activity") %>%
  hc_xAxis(title = list(text = "Activity")) %>%
  hc_yAxis(title = list(text = "Frequency"))

#activity trend
stravata %>%
  mutate(Month = format(ActivityDate, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(count = n()) %>%
  hchart("line", hcaes(x = Month, y = count)) %>%
  hc_title(text = "Number of Activities Over Time") %>%
  hc_xAxis(title = list(text = "Month")) %>%
  hc_yAxis(title = list(text = "Number of Activities"))

stravata %>%
  group_by(ActivityType) %>%
  summarise(avg_maxheartrate = mean(MaxHeartRate, na.rm = TRUE)) %>%
  hchart("column", hcaes(x = ActivityType, y = avg_maxheartrate)) %>%
  hc_title(text = "Average max heart rate by Activity Type") %>%
  hc_xAxis(title = list(text = "Activity Type")) %>%
  hc_yAxis(title = list(text = "Average max heart rate"))


#Dashboard 1 ####
library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)
library(dplyr)
library(lubridate)

# # Define UI for application
# ui <- dashboardPage(
#   dashboardHeader(title = "Strava Analysis"),
#   dashboardSidebar(
#     # Add filters to the sidebar
#     dateRangeInput("dateRange", "Date range:", start = min(stravata$ActivityDate), end = max(stravata$ActivityDate)),
#     selectInput("activityType", "Activity type:", choices = unique(stravata$ActivityType), selected = unique(stravata$ActivityType)[1])
#   ),
#   dashboardBody(
#     # Apply a theme
#     shinyUI(fluidPage(theme = shinytheme("cerulean"),
#                       # Create two tabs
#                       tabsetPanel(
#                         tabPanel("Scatter Plots", 
#                                  fluidRow(
#                                    box(highchartOutput("plot1", height = "500px")),
#                                    box(highchartOutput("plot2", height = "500px"))
#                                  )),
#                         tabPanel("Bar Plots", 
#                                  fluidRow(
#                                    box(highchartOutput("plot3", height = "500px")),
#                                    box(highchartOutput("plot4", height = "500px"))
#                                  ))
#                       )
#     ))
#   )
# )
# 
# # Server logic
# server <- function(input, output) {
#   
#   # Filter data based on inputs
#   filteredData <- reactive({
#     stravata %>%
#       filter(ActivityDate >= input$dateRange[1] & ActivityDate <= input$dateRange[2]) %>%
#       filter(ActivityType == input$activityType)
#   })
#   
#   output$plot1 <- renderHighchart({
#     hchart(filteredData(), "scatter", hcaes(x = ElapsedTime, y = MaxHeartRate)) %>%
#       hc_title(text = "Elapsed Time vs Max Heart Rate") %>%
#       hc_xAxis(title = list(text = "Elapsed Time (seconds)")) %>%
#       hc_yAxis(title = list(text = "Max Heart Rate"))
#   })
#   
#   output$plot2 <- renderHighchart({
#     hchart(filteredData(), "scatter", hcaes(x = ElapsedTime, y = Distance)) %>%
#       hc_title(text = "Elapsed Time vs Distance") %>%
#       hc_xAxis(title = list(text = "Elapsed Time (seconds)")) %>%
#       hc_yAxis(title = list(text = "Distance"))
#   })
#   
#   output$plot3 <- renderHighchart({
#     filteredData() %>%
#       group_by(ActivityType) %>%
#       summarise(count = n()) %>%
#       hchart("column", hcaes(x = ActivityType, y = count)) %>%
#       hc_title(text = "Frequency of Activity Types") %>%
#       hc_xAxis(title = list(text = "Activity Type")) %>%
#       hc_yAxis(title = list(text = "Frequency"))
#   })
#   
#   output$plot4 <- renderHighchart({
#     filteredData() %>%
#       group_by(ActivityType) %>%
#       summarise(avg_distance = mean(Distance, na.rm = TRUE)) %>%
#       hchart("column", hcaes(x = ActivityType, y = avg_distance)) %>%
#       hc_title(text = "Average Distance by Activity Type") %>%
#       hc_xAxis(title = list(text = "Activity Type")) %>%
#       hc_yAxis(title = list(text = "Average Distance"))
#   })
#   
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)


#ai try dashboard 2.0####
library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)

ui <- dashboardPage(
  dashboardHeader(title = "Strava Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Virtual Ride", tabName = "virtual_ride", icon = icon("person-biking")),
      menuItem("Ride", tabName = "ride", icon = icon("bicycle")),
      menuItem("Run", tabName = "run", icon = icon("running")),
      menuItem("Weight Training", tabName = "weight_training", icon = icon("dumbbell")),
      dateRangeInput("dateRange", "Date range:", start = min(stravata$ActivityDate), end = max(stravata$ActivityDate))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "virtual_ride",
              fluidRow(
                box(highchartOutput("virtual_ride_plot1")),
                box(highchartOutput("virtual_ride_plot2"))
              )
      ),
      tabItem(tabName = "ride",
              fluidRow(
                box(highchartOutput("ride_plot1")),
                box(highchartOutput("ride_plot2"))
              )
      ),
      tabItem(tabName = "run",
              fluidRow(
                box(highchartOutput("run_plot1")),
                box(highchartOutput("run_plot2"))
              )
      ),
      tabItem(tabName = "weight_training",
              fluidRow(
                box(highchartOutput("weight_training_plot1")),
                box(highchartOutput("weight_training_plot2"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
# Filter data based on inputs
    filteredData <- reactive({
      stravata %>%
        filter(ActivityDate >= input$dateRange[1] & ActivityDate <= input$dateRange[2])})
  
  output$virtual_ride_plot1 <- renderHighchart({
    virtual_ride_data <- filteredData()[stravata$ActivityType == "Virtual Ride", ]
    hchart(virtual_ride_data, "scatter", hcaes(x = Distance, y = AverageWatts)) %>%
      hc_title(text = "Watts over Time for Virtual Rides")
  })
  
  output$virtual_ride_plot2 <- renderHighchart({
    virtual_ride_data <- filteredData()[stravata$ActivityType == "Virtual Ride", ]
    hchart(virtual_ride_data, "scatter", hcaes(x = Distance, y = Calories)) %>%
      hc_title(text = "Calories over Time for Virtual Rides")
  })
  
  output$ride_plot1 <- renderHighchart({
    ride_data <- filteredData()[stravata$ActivityType == "Ride", ]
    hchart(ride_data, "scatter", hcaes(x = Distance, y = AverageWatts)) %>%
      hc_title(text = "Watts over Time for Rides")
  })
  
  output$ride_plot2 <- renderHighchart({
    ride_data <- filteredData()[stravata$ActivityType == "Ride", ]
    hchart(ride_data, "scatter", hcaes(x = Distance, y = Calories)) %>%
      hc_title(text = "Calories over Time for Rides")
  })
  
  output$run_plot1 <- renderHighchart({
    run_data <- filteredData()[stravata$ActivityType == "Run", ]
    hchart(run_data, "scatter", hcaes(x = Distance, y = Calories)) %>%
      hc_title(text = "Calories over Distance for Runs")
  })
  
  output$run_plot2 <- renderHighchart({
    run_data <- filteredData()[stravata$ActivityType == "Run", ]
    hchart(run_data, "scatter", hcaes(x = Distance, y = ElapsedTime)) %>%
      hc_title(text = "Calories over Distance for Runs")
  })
  
  output$weight_training_plot1 <- renderHighchart({
    weight_training_data <- filteredData()[stravata$ActivityType == "Weight Training", ]
    hchart(weight_training_data, "scatter", hcaes(x = ElapsedTime, y = MaxHeartRate)) %>%
      hc_title(text = "Max Heart Rate over Time for Weight Training")
  })
  
  output$weight_training_plot2 <- renderHighchart({
    weight_training_data <- filteredData()[stravata$ActivityType == "Weight Training", ]
    hchart(weight_training_data, "scatter", hcaes(x = ElapsedTime, y = Calories)) %>%
      hc_title(text = "Calories over Time for Weight Training")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)