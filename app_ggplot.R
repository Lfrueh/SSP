library(shiny)
library(plotly)
library(sf)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggmap)

#Data
data <- read_rds("data/county_2010_12_sf.rds") %>% st_cast(., "MULTIPOLYGON")

# Create selection options -----
state_val <- unique(data$state.abb)

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
                  choices = c("County"),
                              #,"ZCTA", "Census Tracts"),
                  multiple = FALSE),
      selectInput(inputId = "seg_input",
                  label = "Select a Segregation Measure",
                  choices = ice_val,
                  multiple = FALSE),
      selectInput(inputId = "state_input",
                  label = "Select a State",
                  choices = state_val,
                  selected = "PA",
                  multiple = FALSE
      ),
      selectInput(inputId = "county_input",
                  label = "Select a County",
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


  #Eventually a reactive expression would go here selecting data from 
  #county, zcta, or tract sf objects.

  filtered_data <- reactive({
    req(input$year_input, input$state_input)
    data %>%
      filter(state.abb %in% input$state_input & year %in% input$year_input)
  })
  
  
#Get the bounding box  
  bbox <- reactive({
    filtered_data() %>%
      st_buffer(dist = 0.15) %>%
      st_bbox()
  })
  
  output$map <- renderPlotly({
    xlims <- c(bbox()$xmin, bbox()$xmax)
    ylims <- c(bbox()$ymin, bbox()$ymax)
    
    # Calculate the center of the bounding box
    center_lon <- mean(xlims)
    center_lat <- mean(ylims)
    
    ggplotly(
      ggplot(filtered_data()) +
        geom_sf(aes(fill = get(input$seg_input), text = paste(county.name,":",round(get(input$seg_input), 2))), color = "gray", size = 0.1, inherit.aes=FALSE) +
        coord_sf(xlim = xlims, ylim = ylims, expand = TRUE) + 
        scale_fill_gradient2(low = "orange", mid = "white", high = "blue", midpoint = 0),
      tooltip = "text"
    )
    
  })
}

# Run the application -----
shinyApp(ui = ui, server = server)





