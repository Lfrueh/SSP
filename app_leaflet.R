library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(sf)
library(plotly)
library(mapboxapi)
library(leaflet)

mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
mb_access_token(mapbox_token)

county_data <- read_rds("data/county_2010_12_sf.rds")
zcta_data <- read_rds("data/zcta_2010_12_sf.rds")
tract_data <- read_rds("data/tract_2010_12_sf.rds")

# Create selection options -----
state_val <- state.abb

ice_val <- c(
    "ICE Income" = "ICEincome",
    "ICE Race/ethnicity" = "ICEraceeth",
    "ICE Homeownership" = "ICEhome",
    "ICE Income + Race" = "ICEincwb",
    "ICE Income + Race/ethnicity" = "ICEincwnh",
    "ICE Education" = "ICEedu",
    "ICE Language" = "ICElanguage"
)

# User interface -----
ui <- fluidPage(
    titlePanel("Index of Concentration at the Extremes"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "year_input",
                        label = "Select a Year",
                        choices = c("2010", "2011", "2012"),
                        multiple = FALSE),
            selectInput(inputId = "geo_input",
                        label = "Select Geography",
                        choices = c("County","ZCTA", "Census Tracts"),
                        multiple = FALSE,
                        selected = "County"),
            selectInput(inputId = "seg_input",
                        label = "Select a Segregation Measure",
                        choices = ice_val,
                        multiple = FALSE),
            selectInput(inputId = "state_input",
                        label = "Select a State",
                        choices = state_val,
                        selected = "PA",
                        multiple = FALSE),
            selectInput(inputId = "county_input",
                        label = "Select Counties",
                        choices = NULL,
                        multiple = TRUE)
        ),
        # Show map
        mainPanel(
           plotlyOutput(outputId = "map")
        )
    )
)


# Server -----



server <- function(input, output, session) {
  data <- reactive({
    if(input$geo_input=="County"){
      imported_data <- county_data
    } else if(input$geo_input=="ZCTA"){
      imported_data <- zcta_data
    } else if(input$geo_input=="Census Tracts"){
      imported_data <- tract_data
    }
    imported_data %>% 
      filter(year == input$year_input & state.abb == input$state_input) %>%
      st_make_valid(.)
  })
  
  pal = colorNumeric(
    palette = c("orange", "white", "blue"),
    domain = c(-1, 1))
  
  output$map = renderPlotly({
    plot_mapbox(
      data = data(),
      split = ~GEOID10,
      fillcolor = ~pal(ICEincome),
      opacity = 0.5,
      hoverinfo = "text",
      text = ~paste(county.name, ": ", round(ICEincome, 2)),
      showlegend = FALSE,
      stroke = FALSE)
  })
}

# Run the application -----
shinyApp(ui = ui, server = server)

