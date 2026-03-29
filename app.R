# app.R
library(shiny)
library(bslib)
library(bsicons)
library(plotly)

# 1. Run the data processing script
source("data_cleaning.R")

# 2. Load the UI and Server modules for each page
source("demo.R")
source("geo.R")
source("forecast.R")

# 3. Assemble the Main User Interface
ui <- page_navbar(
    title = "Connecticut Drug Overdoses (2012-2018)",
    theme = bs_theme(bootswatch = "flatly", bg = "#f8f9fa", fg = "#212529"),
    
    # Plug in the UIs from the other files
    demo_ui,
    geo_ui,
    forecast_ui
)

# 4. Assemble the Main Server Logic
server <- function(input, output, session) {
    
    # Plug in the servers from the other files
    demo_server(input, output, session)
    geo_server(input, output, session)
    forecast_server(input, output, session)
}

# 5. Run the App
shinyApp(ui = ui, server = server)