library(httr)
library(jsonlite)
library(tidyverse)

build_weather_data <- function(
    latitude,
    longitude,
    start_date,
    end_date,
    daily = c("apparent_temperature_max", 
              "apparent_temperature_min",
              "precipitation_sum",
              "rain_sum",
              "snowfall_sum",
              "daylight_duration"),
    timezone = "America/New_York",
    temp_unit = "fahrenheit",
    precip_unit = "inch")
{
  

  base_url <- "https://archive-api.open-meteo.com/v1/archive?"
  
  is_valid_date <- function (dates){
      grepl("^\\d{4}-\\d{2}-\\d{2}$", dates)
    }
    if (!is_valid_date(start_date)){
      stop("start_date must be in 'yyyy-mm-dd' format")
    }
    if (!is_valid_date(end_date)){
      stop("end_date must be in 'yyyy-mm-dd' format")
    }
  
  daily_entry <- paste0("daily=", paste(daily, collapse = ","), "&")
  
  timezone <- URLencode(timezone, reserved = TRUE)

  url <- paste0(base_url,
                "latitude=", latitude, "&",
                "longitude=", longitude, "&",
                "start_date=", start_date, "&",
                "end_date=", end_date, "&",
                daily_entry,
                "timezone=auto&",
                "temperature_unit=", temp_unit, "&",
                "precipitation_unit=", precip_unit, "&")
  
weather_history_url <- GET(url)

parsed_weather_history <- (fromJSON(rawToChar(weather_history_url$content)))

#our target is the daily_units and the daily columns which are currently lists
#extract each column below
daily_units <- parsed_weather_history$daily_units
daily_data <- parsed_weather_history$daily

# Convert daily_data to tibble 
daily_df <- as_tibble(daily_data)

# Create a named vector for units with same names as daily_data columns
units_vec <- unlist(daily_units)

# Now rename columns by appending units in parentheses, e.g. "temperature_max (°F)"
names(daily_df) <- paste0(names(daily_df), " (", units_vec[names(daily_df)], ")")

weather_results <- as_tibble(daily_df)

return(weather_results)


 }

#using to test function()
build_weather_data(
    latitude = 36.07,
    longitude = -79.79,
    start_date = "2024-06-01",
    end_date = "2024-06-05",
    daily = c("apparent_temperature_max", 
              "apparent_temperature_min",
              "precipitation_sum",
              "rain_sum",
              "snowfall_sum",
              "daylight_duration"),
    timezone = "America/New_York",
    temp_unit = "celsius",
    precip_unit = "mm")

