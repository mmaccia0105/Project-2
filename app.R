library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(preset = "darkly"),
    
    navset_pill(
      nav_panel("About this App", 
                tagList(
                  h2("Purpose of this App"),
                  p("The purpose of this application is to explore some historical weather data. 
       Locations can be input to explore various factors associated with weather from 1940 to the present."),
                  
                  h2("Source of the Data"),
                  p(HTML('This data comes from <a href="https://open-meteo.com/" target="_blank">Open-meteo</a>, 
           an open-source API that includes current, forecast, and historical weather data. 
           This app focuses on historical data available from 1940 onwards. It’s a free API that does not require a key.')),
                  
                  h2("App Contents"),
                  
                  h4("About the App Tab"),
                  p("This main tab has information to explain the purpose of this app."),
                  
                  h4("Data Download Tab"),
                  p("This tab will allow you to input various parameters to access data from the API. The available inputs are:"),
                  
                  h4("Exploring the Data Tab"),
                  p("This tab will allow you to see various data outputs based on the input data.")
                )),
                
      nav_panel("Data Download", 
            sidebarLayout(
              sidebarPanel(
                numericInput("latitude",
                             "Latitude:",
                             min = -180, max = 180,
                             value = 36.07),
                numericInput("latitude",
                             "Latitude:",
                             min = -180, max = 180,
                             value = 79.79),
                textInput("start_date", 
                          "Start Date (yyyy-mm-dd):",
                          value = "2025-01-01"),
                textInput("end_date", 
                          "End Date (yyyy-mm-dd):",
                          value = "2025-04-01"),
                selectInput("daily", "Daily Values",
                            choices = c("Maximum Temperature" = "apparent_temperature_max", 
                                        "Minimum Temperature" = "apparent_temperature_min",
                                        "Total Precepitation" = "precipitation_sum",
                                        "Total Rain" = "rain_sum",
                                        "Total Snowfall" = "snowfall_sum",
                                        "Daylight Duration" = "daylight_duration"),
                            selected = "apparent_temperature_max",
                            multiple = TRUE),
                selectInput("temp_unit", "Temperature Unit",
                            choices = c("Fahrenheit" = "fahrenheit",
                                        "Celsius" = "celsius"),
                            selected = "fahrenheit"),
                selectInput("precip_unit", "Precipitation Unit",
                            choices = c("Imperial" = "inch",
                                        "Metric" = "mm"),
                            selected = "inch"),
                actionButton("get_data", "Get Weather Data")
              ),
              mainPanel(
                h3("Returned Weather Data")
                
              )
            )),
      nav_panel("Exploring the Data", "Here we will have 4 outputs")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
 
}

# Run the application 
shinyApp(ui = ui, server = server)
