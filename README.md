# Project 2 – Creating a Shiny App

## **Author:** Mike Maccia

## Description of App

This app was created to gather data from an API and allow the user to manipulate data inputs, download the data, and provide some visual outputs.

The app allows the user to explore some historical weather data. Locations can be input to explore various endpoints from 1940 to the present. Within the app, there are multiple different options for the user to output and visualize data.

This data comes from [Open-Meteo](https://open-meteo.com/), an open-source API that includes current, forecast, and historical weather data. This app focuses on historical data available from 1940 onwards. It’s a free API that does not require a key.

------------------------------------------------------------------------

## Packages Needed to Run the App

-   `shiny`

-   `shinycssloaders`

-   `bslib`

-   `httr`

-   `jsonlite`

-   `tidyverse`

-   `DT`

## Code to Install Necessary Packages

\`\`\`{r} install.packages(c( "shiny", "shinycssloaders", "bslib", "httr", "jsonlite", "tidyverse", "DT"))

## Running the APP

\`\`\`{r} shiny::runGitHub("Project-2", "mmaccia0105")
