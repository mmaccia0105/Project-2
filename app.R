#__________________________Packages to Load for APP_________________
library(shiny)
library(shinycssloaders)
library(bslib)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)

#__________________________Function to Get Data from API_____________

#choosing some of the variables available
build_weather_data <- function(
    latitude,
    longitude,
    start_date,
    end_date,
    #combining multiple weather data that is availabe
    daily = c("apparent_temperature_max", 
              "apparent_temperature_min",
              "precipitation_sum",
              "rain_sum",
              "snowfall_sum",
              "daylight_duration"),
    #timezone is default, doesn't affect output so defaulting to the below
    timezone = "America/New_York",
    temp_unit = "fahrenheit",
    precip_unit = "inch")
{
  
  #URL to get API
  base_url <- "https://archive-api.open-meteo.com/v1/archive?"
  
  #checking the data to makes sure data is correct format
  is_valid_date <- function (dates){
    grepl("^\\d{4}-\\d{2}-\\d{2}$", dates)
  }
  if (!is_valid_date(start_date)){
    stop("start_date must be in 'yyyy-mm-dd' format")
  }
  if (!is_valid_date(end_date)){
    stop("end_date must be in 'yyyy-mm-dd' format")
  }
  #using paste to allow for the multiple inputs of the daily argument
  daily_entry <- paste0("daily=", paste(daily, collapse = ","), "&")
  
  #taking the timezone and converting it to HTML format
  timezone <- URLencode(timezone, reserved = TRUE)
  
  #building the URL from inputs
  url <- paste0(base_url,
                "latitude=", latitude, "&",
                "longitude=", longitude, "&",
                "start_date=", start_date, "&",
                "end_date=", end_date, "&",
                daily_entry,
                "timezone=auto&",
                "temperature_unit=", temp_unit, "&",
                "precipitation_unit=", precip_unit, "&")
  
  #connecting to the API
  weather_history_url <- GET(url)
  
  #converting the API data from JSON
  parsed_weather_history <- (fromJSON(rawToChar(weather_history_url$content)))
  
  #results provided indicate date, location, and time of entry
  #the target is the daily_units and the daily columns which are currently lists
  #extract each column below - but we do want the date column to remain
  daily_units <- parsed_weather_history$daily_units
  daily_data <- parsed_weather_history$daily
  
  # Convert daily_data to tibble 
  daily_df <- as_tibble(daily_data)
  
  # Create a named vector for units with same names as daily_data columns
  units_vec <- unlist(daily_units)
  
  #convert daily duration from seconds to hours
  if ("daylight_duration" %in% names(daily_df)) {
    daily_df <- daily_df |> 
      mutate(daylight_duration = round(daylight_duration / 3600, 2))
    units_vec["daylight_duration"] <- "hr"
  }
  #converting time(iso8601) to date
  names(daily_df)[1] <- "Date"
  
  # Now rename columns by changing units in parentheses, e.g. "temperature_max (°F)"
  names(daily_df) <- paste0(names(daily_df), " (", units_vec[names(daily_df)], ")")
  
  #put everything into a tibble
  weather_results <- as_tibble(daily_df)
  
  #return the dataset
  return(weather_results)
  
}

#_________________Create a function to categorize temperature data______
categorize_temperature <- function(temp, unit="fahrenheit") {
  #converting to numeric
  temp <- as.numeric(temp)
  #define ranges for Fahrenheit first
  if (unit == "fahrenheit") {
    cut(temp,
        breaks = c (-Inf, 32, 50, 65, 75, Inf),
        labels = c("Very Cold", "Cold", "Chilly", "Warm", "Hot"),
        right = FALSE)
  } else {
    #celsius 
    cut(temp,
        breaks = c (-Inf, 0, 10, 18, 24, Inf),
        labels = c("Very Cold", "Cold", "Chilly", "Warm", "Hot"),
        right = FALSE)
  }
  
}

#___________________Create a function to categorize precip sum_________
categorize_precip <- function(precip, unit = "inch"){
  precip <- as.numeric(precip)
  #build similar to temp above
  if (unit == "inch") {
    cut(precip,
        breaks = c(-Inf, 0.1, 0.5, 1, 2, Inf),
        labels = c("None/Trace", "Light", "Moderate", "Heavy", "Very Heavy"),
        right = FALSE)
  } else {
    #metric is in mm
    cut(precip,
        breaks = c(-Inf, 0.1, 0.5, 1, 2, Inf),
        labels = c("None/Trace", "Light", "Moderate", "Heavy", "Very Heavy"),
        right = FALSE)
  }
  
}
#___________________Build APP______________________________


#___________________UI_____________________________________
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  
  navset_pill(
    nav_panel("About this App", 
              tagList(
                h2("Purpose of this App"),
                p("The purpose of this application is to explore some historical weather data. 
       
                    Locations can be input to explore various endpoints from 1940 to the present. 
                    Within the app, there will be multiple different options for the user to output data."),
                
                h2("Source of the Data"),
                p(HTML('This data comes from <a href="https://open-meteo.com/" target="_blank">Open-meteo</a>, 
                  an open-source API that includes current, forecast, and historical weather data. 
                  This app focuses on historical data available from 1940 onwards. It’s a free API that does not require a 
                  key.')),
                
                h2("App Contents"),
                
                h4("About the App Tab"),
                p("This main tab has information to explain the purpose of this app."),
                
                h4("Data Download Tab"),
                p("This tab will allow you to input various parameters to access data from the API. The available inputs 
                    are:"),
                tags$ul(
                  tags$li(strong("Location –"), " The latitude and longitude are required as inputs. Since the coordinates
                            are not common knowledge, the coordinates for a target location can be found using online tools 
                            like", tags$a(href = "https://www.gps-coordinates.net/", "GPS Coordinates", target = "_blank"), 
                          "."),
                  tags$li(strong("Dates –"), ' The start and end date for historical data can be input. The required 
                            format is "YYYY-MM-DD".'),
                  tags$li(strong("Daily Weather Variables –"), " You can select any combination of the following:"),
                  tags$ul(
                    tags$li("Minimum Daily Temperature"),
                    tags$li("Maximum Daily Temperature"),
                    tags$li("Total Precipitation"),
                    tags$li("Total Rainfall"),
                    tags$li("Total Snowfall"),
                    tags$li("Duration of Daylight")
                  ),
                  tags$li(strong("Temperature Units –"), " Fahrenheit and Celsius are options."),
                  tags$li(strong("Precipitation Units –"), " Imperial or Metric are options.")
                ),
                p("After selected the desired outputs, data will be displayed. There are additional options to subset some
                    of the chosen variables and adjust the number of rows display. Additionally, there is an option to 
                    download the data to a '.csv' file."),
                
                
                h4("Exploring the Data Tab"),
                p("This tab will allow you to see various data outputs based on the input data."),
                tags$ul(
                  tags$li(
                    strong("Contingency Table:"), 
                    tags$ul(
                      tags$li("User will be able to get frequency counts for various returned data. User can toggle between 
                              different data points as desired.")
                    )
                  ),
                  tags$li(
                    strong("Summary Statisitics Table:"),
                    tags$ul(
                      tags$li("The user will be able to output the mean, median, minimum, and maximum for the various data 
                              chosen. User will be able to toggle between different data points as desired.")
                    )
                  )),
                
              ),
              tags$img(
                src = "weather_image.png",
                alt = "Weather Image",
                width = "400px"
              )
    ),
    
    
    nav_panel("Data Download", 
              #sidebar will have the input options for data retrieval
              sidebarLayout(
                sidebarPanel(
                  #Since need to use coordinates, will ask user to enter location so can use for outputs
                  textInput("target_location",
                            "Target Location",
                            value = "Greensboro, NC (Enter Target Location)"),
                  #Latitude and Longitude Inputs
                  numericInput("latitude",
                               "Latitude:",
                               min = -180, max = 180,
                               value = 36.07),
                  numericInput("longitude",
                               "Longitude:",
                               min = -180, max = 180,
                               value = -79.79),
                  #Date inputs
                  textInput("start_date", 
                            "Start Date (yyyy-mm-dd):",
                            value = "2025-01-01"),
                  textInput("end_date", 
                            "End Date (yyyy-mm-dd):",
                            value = "2025-04-01"),
                  #Add a multiple choice list to the available daily values
                  #rename them with easy to read labels
                  selectInput("daily", "Daily Values",
                              choices = c("Maximum Temperature" = "apparent_temperature_max", 
                                          "Minimum Temperature" = "apparent_temperature_min",
                                          "Total Precepitation" = "precipitation_sum",
                                          "Total Rain" = "rain_sum",
                                          "Total Snowfall" = "snowfall_sum",
                                          "Daylight Duration" = "daylight_duration"),
                              selected = "apparent_temperature_max",
                              multiple = TRUE),
                  #choose the units for temp
                  selectInput("temp_unit", "Temperature Unit",
                              choices = c("Fahrenheit" = "fahrenheit",
                                          "Celsius" = "celsius"),
                              selected = "fahrenheit"),
                  #precip units - rain is mm, snow is cm, so will use metric and imperial 
                  selectInput("precip_unit", "Precipitation Unit",
                              choices = c("Imperial" = "inch",
                                          "Metric" = "mm"),
                              selected = "inch"),
                  #add an action button so user will tell app when ready to generate data
                  actionButton("get_data", "Get Weather Data")
                ),
                #main panel for data output, reactive text output based on user input
                mainPanel(
                  h3(textOutput("target_location_header")),
                  #adding the weather output with subset options
                  #spinner below to indicate working on data
                  uiOutput("subset_weather"),
                  withSpinner(dataTableOutput("weather_table"), type = 5, color = "green"),
                  #adding a download button to output the data
                  downloadButton("download_weather",
                                 "Download Data")
                  
                )
              )),
    nav_panel("Exploring the Data",
              #where user can choose the outputs
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput("selected_vars", "Select Variable to Count:",
                                     choices = NULL),
                  #add an action button so user will tell app when ready to generate data
                  actionButton("get_contingency", "Get Selected Counts"),
                  tags$hr(),
                  #summary tables
                  selectInput("num_vars", "Select Numeric Variables to Summarize:",
                              choices = NULL, multiple = TRUE),
                  checkboxGroupInput("group_vars", "Select Grouping Variables:",
                                     choices = NULL),
                  actionButton("get_summary", "Get Selected Statistics"),
                  tags$hr(),
                  #action button so histogram doesn't automatically appear or cause error without data
                  actionButton("get_histogram", "Get Histogram for Temperature Categories"),
                  tags$hr(),
                  #set up user choices for a scatter plot looking at temps vs daylight duration
                  selectInput("temp_scatter", "Temperature Variable:",
                              choices = c("Max Temperature" = "apparent_temperature_max",
                                          "Min Temperature" = "apparent_temperature_min"),
                              selected = "apparent_temperature_max"),
                  selectInput("facet_by_scatter", "Facet By:",
                              choices = c("Quarter", "Year"),
                              selected = "Quarter"),
                  actionButton("get_scatter", "Get Precipitation Scatter Plot"),
                  tags$hr(),
                  checkboxGroupInput("line_temp_vars", "Select Temperature Variables to Plot:",
                                     choices = c("Max Temperature" = "apparent_temperature_max",
                                                 "Min Temperature" = "apparent_temperature_min"),
                                     selected = "apparent_temperature_max"),
                  checkboxGroupInput("line_precip_vars", "Select Precipitation Variables to Plot:",
                                     choices = c("Total Precipitation" = "precipitation_sum",
                                                 "Total Rain" = "rain_sum",
                                                 "Total Snowfall" = "snowfall_sum"),
                                     selected = NULL),
                  actionButton("get_line_plot", "Plot Temperature & Precipitation Over Time"),
                  tags$hr(),
                  selectInput("heatmap_temp_choice", "Select Temperature to Display:",
                              choices = c("Maximum Temperature" = "apparent_temperature_max",
                                          "Minimum Temperature" = "apparent_temperature_min"),
                              selected = "apparent_temperature_max"),
                  actionButton("plot_heatmap", "Generate Heatmap")
                  
                ),
                #main panel will have data outputs
                mainPanel(
                  h1(textOutput("data_page_header")),
                  h3(textOutput("target_location_contingency")),
                  dataTableOutput("contingency_table"),
                  h3(textOutput("target_location_summary")),
                  dataTableOutput("summary_table"),
                  h3(textOutput("target_location_histogram")),
                  withSpinner(plotOutput("temp_cat_histogram"), type= 4, color = "green"),
                  h3(textOutput("target_location_scatter")),
                  withSpinner(plotOutput("trend_scatter"), type= 4, color = "green"),
                  h3(textOutput("target_location_line")),
                  withSpinner(plotOutput("line_plot"), type= 4, color = "green"),
                  h3(textOutput("heatmap_header")),
                  withSpinner(plotOutput("temp_heatmap"), type = 4, color = "blue"),
                )
              )
    )
  )
)

#_______________________SERVER_________________________________
server <- function(input, output, session) {
  #_____building the data from input, making it reactive______
  weather_data <- eventReactive(input$get_data, {
    req(input$latitude, input$longitude, input$start_date, input$end_date, input$daily)
    
    data <-  build_weather_data(
      latitude = input$latitude,
      longitude = input$longitude,
      start_date = input$start_date,
      end_date = input$end_date,
      daily = input$daily,
      timezone = "America/New_York",
      temp_unit = input$temp_unit,
      precip_unit = input$precip_unit
    )
    #create vectors for the temp we are going to categorize
    temp_vars <- c("apparent_temperature_max", "apparent_temperature_min")
    
    #extracting out the columns for temp to categorize, if chosen as an input
    if (any(temp_vars %in% input$daily)){
    temp_max_col <- grep("^apparent_temperature_max.*", names(data), value = TRUE)
    temp_min_col <- grep("^apparent_temperature_min.*", names(data), value = TRUE)
   
    #apply the categories for temp columns
    if (length(temp_max_col) > 0) {
      data$temp_category <- categorize_temperature(data[[temp_max_col]], input$temp_unit)
    } else if (length(temp_min_col) > 0) {
      data$temp_category <- categorize_temperature(data[[temp_min_col]], input$temp_unit)
    } else {
      data$temp_category <- NULL
    }
    }
    #do the same for precipitation sum but not for rainfall/snowfall totals
    if ("precipitation_sum" %in% input$daily) {
      precip_col <- grep("^precipitation_sum.*", names(data), value = TRUE) 
      if (length(precip_col) > 0) {
        data$precip_category <- categorize_precip(data[[precip_col]], unit = input$precip_unit)
      } else {
        data$precip_category <- NULL
      }
    }
  
    # Find the date column name 
    date_col <- grep("^Date", names(data), value = TRUE)
    
    #get rid of date units
    data$date_clean <- as.Date(data[[date_col]])
    
    #put year month day intodifferent columns
    data$Year <- format(data$date_clean, "%Y")
    data$Month <- format(data$date_clean, "%B")
    data$Day <- format(data$date_clean, "%d")
    #make the dates factors
    data$Month <- factor(data$Month, 
                         levels = c("January", "February", "March", "April", "May", "June", 
                                    "July", "August", "September", "October", "November", "December"),
                         ordered = TRUE)
    
    data$Day <- factor(data$Day, levels = sprintf("%02d", 1:31), ordered = TRUE)
    data$Year <- factor(data$Year, ordered = FALSE)
    
    #drop the original date column
    data[[date_col]] <- NULL
    data$date_clean <- NULL
    
    #reorder date to front
    data <- data |> 
      relocate(Month, Day, Year)
    
    return(data)
  })
  
  
  #reactive output to the page for the data
  output$target_location_header <- renderText({
    req(input$get_data)
    req(input$target_location)
    paste("Historical Weather Data for:", input$target_location)
  })
  #allowing for reactive subsetting of data after it generates
  output$subset_weather <- renderUI({
    req(weather_data())
    checkboxGroupInput("subset_weather", "Select Columns to Display:",
                       choices = names(weather_data()),
                       selected = names(weather_data()))
  })
  #making the subset data reactive
  subset_weather_data <- reactive({
    req(weather_data())
    data <- weather_data()
    #checking if anthing is checked to subset 
    if (!is.null(input$subset_weather)) {
      data <- data |> 
        select(all_of(input$subset_weather))
    }
    #return data
    return(data)
  })
  
  #adjusting the choices for subsetting - making sure data is an option
  output$weather_table <- renderDataTable({
    data <- subset_weather_data()
    if (ncol(data) > 0) {
      names(data)[1] <- "Date"
    }
    data
  })
  #download output - will download to a csv file
  output$download_weather <- downloadHandler(
    filename = function(){
      paste0("weather", input$start_date,"_to_", input$end_date,".csv")
    },
    content = function(file) {
      write_csv(subset_weather_data(), file)
    }
  )
  #________________Contingency table________
  #Adding reactive header with location and dates
  output$data_page_header <- renderText({
    req(input$target_location)
    paste("Data Outputs for", input$target_location, "from", input$start_date, "to", input$end_date)
  })

  observeEvent(weather_data(), {
    data <- weather_data()
    vars <- names(data)
    #checking the variables are categorical before moving forward
    cat_vars <- vars[sapply(weather_data(), function(col) is.factor(col) || is.character(col))]
    #make checkbox based on available variables
    updateCheckboxGroupInput(inputId = "selected_vars", choices = cat_vars)
  })  
  
  #Contingency table output
  contingency_table_data <- eventReactive(input$get_contingency, {
    req(input$selected_vars)
    data <- weather_data()
    #selected for data that is categorical
    selected_data_freq <- data[, input$selected_vars, drop = FALSE]
    #making the freq table, do.call was needed since had a list of variables
    freq_table <- as.data.frame(
      do.call(table, c(as.list(selected_data_freq), useNA = "ifany"))
    ) |>
      filter(Freq > 0)
    
    return(freq_table)
  })
  
  # Render the contingency table only after button is clicked
  output$contingency_table <- renderDataTable({
    req(input$get_contingency > 0)
    contingency_table_data()
  })
  
  # Similarly for header text output, triggered by button
  output$target_location_contingency <- renderText({
    req(input$get_contingency > 0)
    paste("Selected Counts for:", input$target_location)
  })
  
  
  #__________________Summary Table________________________ 
  #reactive output to title the table
  output$target_location_summary <- renderText({
    req(input$get_summary > 0)
    req(input$target_location)
    paste("Selected Statistics for:", input$target_location)
  })
  
  observeEvent(weather_data(), {
    data <- weather_data()
    
    # Identify numeric variables for summarization
    num_vars <- names(data)[sapply(data, is.numeric)]
    
    # Identify categorical variables for grouping
    cat_vars <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
    
    # Update selectInput for numeric vars
    updateSelectInput(session, "num_vars", choices = num_vars, selected = num_vars[1])
    
    # Update checkboxGroupInput for grouping vars
    updateCheckboxGroupInput(session, "group_vars", choices = cat_vars)
  })
  summary_table <- eventReactive(input$get_summary, {
    req(input$num_vars)
    data <- weather_data()
    
    #return empty data frame to avoid errors
    if (nrow(data) == 0) {
      return(data.frame()) 
    }
    # If no grouping variables selected, do a simple summary over whole dataset
    if (is.null(input$group_vars) || length(input$group_vars) == 0) {
      summary_output <- data |>  
        summarise(across(all_of(input$num_vars),
                         list(
                           mean = ~round(mean(.x, na.rm = TRUE), 2), 
                           median = ~round(median(.x, na.rm = TRUE), 2), 
                           min = ~round(min(.x, na.rm = TRUE), 2), 
                           max = ~round(max(.x, na.rm = TRUE), 2)
                         ))) 
      return(summary_output)
    } else {
      # Group by selected grouping variables
      summary_output <- data |> 
        group_by(across(all_of(input$group_vars))) |> 
        summarise(across(all_of(input$num_vars),
                         list(
                           mean = ~round(mean(.x, na.rm = TRUE), 2), 
                           median = ~round(median(.x, na.rm = TRUE), 2), 
                           min = ~round(min(.x, na.rm = TRUE), 2), 
                           max = ~round(max(.x, na.rm = TRUE), 2)
                         )),
                  .groups = "drop") 
    }
    
    return(summary_output)
    
  })
  #output the summary table
  output$summary_table <- renderDataTable({
    req(input$get_summary>0)
    datatable(summary_table())
  })
  
#_______________Histogram_____________________________________

  #add reactive title for histogram based on action button
  output$target_location_histogram <- renderText({
    req(input$get_histogram)
    paste("Number of Days in Each Temperature Category by Year")})
  
  
  #histogram, require the data and the action button for histogram
  output$temp_cat_histogram <- renderPlot({
    req(input$get_histogram)
    req(weather_data())
    ggplot(weather_data(), aes(x = temp_category, fill = Year))+
      geom_bar(position = "dodge") +
      labs(
        x = "Temperature Category",
        y = "Frequency (n days)") +
      theme(axis.text=element_text(size = 16),
            axis.title=element_text(size = 20),
            legend.text=element_text(size = 16),
            legend.title=element_text(size = 20))
  })
#_______________Scatter Plot______________________________________

#add reactive title for scatter based on action button
output$target_location_scatter <- renderText({
    req(input$get_scatter >0)
    paste("Temperature Trends Based on Daylight Duration")})

output$trend_scatter <- renderPlot({
  #action button and data needed
  req(input$get_scatter >0)
  req(weather_data())
  
  
  data <- weather_data()
  #columns for temp choice
  temp_col <- grep(paste0("^", input$temp_scatter, ".*"), names(data), value = TRUE)[1]
  req(!is.null(temp_col))
  #column for daylight duration
  daylight_col <- grep("^daylight_duration.*", names(data), value = TRUE)[1]
  req(!is.null(daylight_col))
  
  #create quarters based on month
  data$Quarter <- factor(quarters(as.Date(paste(data$Year, match(data$Month, month.name), "15", sep = "-"))),
                         levels = c("Q1", "Q2", "Q3", "Q4"))
  
  #select facet variable
  facet_var <- if (input$facet_by_scatter == "Year") {
    sym("Year")
  } else {
    sym("Quarter")
  }
  #make dynamic labels
  x_label <- if (input$temp_scatter == "apparent_temperature_max") {
    "Max Temperature"
  } else {
    "Min Temperature"
  }
  
  unit_label <- if (input$temp_unit == "fahrenheit") {
    " (°F)"
  } else {
    " (°C)"
  }
  
  ggplot(data, aes(x = .data[[temp_col]], y = .data[[daylight_col]])) +
    geom_jitter(color = "red", alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "green") +
    #need to pull in the facet variable chosen 
    facet_wrap(vars(!!facet_var))+
    labs(
      x = paste0(x_label, unit_label),
      y = "Daylight Duration (hr)") +
    theme(axis.text=element_text(size = 16),
          axis.title=element_text(size = 20), 
          strip.text = element_text(size = 16))
      
})

#_____________________LINE PLOT____________________________
#add reactive title for line based on action button
output$target_location_line <- renderText({
  req(input$get_line_plot)
  paste("Temperature and Precipitation Trends Over Time")})

output$line_plot <- renderPlot({
  req(input$get_line_plot >0)
  req(weather_data())
  
  data <- weather_data()
  
  # Reconstruct a Date column from Year, Month, Day
  # Convert Month name to month number
  month_num <- match(data$Month, month.name)
  
  # Create a Date vector
  data$date_clean <- as.Date(
    paste(data$Year, month_num, as.character(data$Day), sep = "-"),
    format = "%Y-%m-%d"
  )
  
  # Prepare variables selected by user
  temp_vars <- input$line_temp_vars
  precip_vars <- input$line_precip_vars
  
  #finding the columns in the datam loop through them
  selected_cols <- c()
  for (i in temp_vars) {
    colname <- grep(paste0("^", i, ".*"), names(data), value = TRUE)
    selected_cols <- c(selected_cols, colname)
  }
  for (j in precip_vars) {
    colname <- grep(paste0("^", j, ".*"), names(data), value = TRUE)
    selected_cols <- c(selected_cols, colname)
  }
  
  req(length(selected_cols) > 0)
  
  #getting data
  plot_data <- data |> 
    select(date_clean, all_of(selected_cols)) |> 
    pivot_longer(cols = -date_clean, names_to = "Variable", values_to = "Value")
  
  #plot
  
  ggplot(plot_data, aes(x = date_clean, y = Value, color = Variable)) +
    geom_line(linewidth = 1) +
    labs(
      x = "Date",
      y = "Value",
      color = "Variable",
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)) 
})

#___________________Heat Map__________________________________________
#make reactive title for heat map
output$heatmap_header <- renderText({
  req(input$plot_heatmap > 0)  
  paste("Average Temperature Heatmap by Year and Month")
})
#get data for heat map
heatmap_data <- eventReactive(input$plot_heatmap, {
  req(weather_data())
  req(input$heatmap_temp_choice)
  
  data <- weather_data()
  #get the temp columns 
  temp_col <- grep(paste0("^", input$heatmap_temp_choice, ".*"), names(data), value = TRUE)[1]
  req(temp_col)
  #calculating the mean temp
  avg_temp <- data |> 
    group_by(Year, Month) |> 
    summarise(AvgTemp = mean(.data[[temp_col]], na.rm = TRUE), .groups = "drop") 
    
  #creating the column for ave temp for month
  avg_temp$Month <- factor(avg_temp$Month, levels = month.name, ordered = TRUE)
  
  avg_temp
})
#reactive heat map
output$temp_heatmap <- renderPlot({
  req(heatmap_data())
  avg_temp <- heatmap_data()
  
  ggplot(avg_temp, aes(x = Month, y = Year, fill = AvgTemp)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "plasma", name = "Avg Temp") +
    labs(x = "Month",
         y = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 16),
          axis.text=element_text(size = 16),
          axis.title=element_text(size = 20))
})

}
#________________Run the APP___________________________________________
shinyApp(ui = ui, server = server)