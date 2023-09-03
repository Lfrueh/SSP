library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(sf)
library(plotly)
library(mapboxapi)
library(leaflet)
library(shinycssloaders)

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
            uiOutput("county_select")
        ),
        # Show map
        mainPanel(
          # Conditional help text
          uiOutput("helptext"),
          withSpinner(
           plotlyOutput(outputId = "map")
          )
        )
    )
)


# Server -----
pal = colorNumeric(
  palette = c("orange", "white", "blue"),
  domain = c(-1, 1))


server <- function(input, output, session) {
  
  # Reactive expression for county choices
  county_choices <- reactive({
    county_data %>% 
      filter(state.abb == input$state_input) %>%
      pull(county.name) %>%
      unique() %>%
      sort()
  })
  
  # Update county selection UI
  output$county_select <- renderUI({
    selectInput(inputId = "county_input",
                label = "Select Counties",
                choices = county_choices(),
                multiple = TRUE)
  })
  
  data <- reactive({
    if(input$geo_input=="County"){
        imported_data <- county_data %>% filter(year == input$year_input & 
                                                  state.abb == input$state_input &
                                                  (is.null(input$county_input) | county.name %in% input$county_input))
    }

      else if(input$geo_input=="ZCTA"){
      imported_data <- zcta_data %>% filter(year == input$year_input & 
                                              state.abb == input$state_input & 
                                              county.name %in% input$county_input) 
    } else if(input$geo_input=="Census Tracts"){
      imported_data <- tract_data %>% filter(year == input$year_input & 
                                               state.abb == input$state_input & 
                                               county.name %in% input$county_input)
    }
    return(imported_data)
  })
  
  
  
  helptext <- reactive({
    if (is.null(data()) || nrow(data()) == 0) {
      if(input$geo_input %in% c("County", "ZCTA") && input$year_input > 2010){
          return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
          Please select at least one county to render map!
                      </p>"))
        }
      else if (input$geo_input == "ZCTA" && input$year_input ==2010){
        return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
          ZCTA Available in years 2011 and later.
                      </p>"))
      }
      else {
        return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
          Please select at least one county to render map!
                      </p>"))
      }
      
    } else if (input$seg_input == "ICEedu" && input$year_input < 2012){
      return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
          ICE Education abailable in years 2012 and later.
                      </p>"))  
    }
      else {
      return(NULL)
    }
  })
  
  # Render the help text
  output$helptext <- renderUI({
    helptext()
  })
  

  output$map = renderPlotly({
    ice_name <- names(ice_val[ice_val == input$seg_input])
    plot_mapbox(
      data = data(),
      split = ~GEOID10,
      fillcolor = ~pal(get(input$seg_input)),
      opacity = 0.5,
      hoverinfo = "text",
      text = ~paste(county.name, ": ", round(get(input$seg_input), 2)),
      showlegend = FALSE,
      stroke = FALSE) %>%
      layout(
        title = paste0(ice_name,": ", 
                       input$county_input, " ",input$state_input),  # Add your desired title
        mapbox = list(
          style = "light",
          colorbar = list(
            title = "Color Bar Title"  # Add your color bar title
          )
        )
      )
  })
}

# Run the application -----
shinyApp(ui = ui, server = server)

