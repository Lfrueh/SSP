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

county_shp <- read_rds("data/county_2010_12_sf.rds") %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
#zcta_shp <- read_rds("data/zcta_2010_12_sf.rds") %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
#tract_shp <- read_rds("data/tract_2010_12_sf.rds")  %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)

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
                        multiple = FALSE
                        )
            #,
             # selectInput(inputId = "county_input",
             #             label = "Select Counties",
             #             choices = NULL,
             #             multiple = TRUE)
        ),
        # Show map
        mainPanel(
           plotlyOutput(outputId = "map")
        )
    )
)


# Server -----



server <- function(input, output, session) {
    # observe({
    #    # Update counties with the selected state
    #      counties <- filtered_data() %>%
    #          filter(state.abb == input$state_input) %>%
    #          pull(county.name) %>%
    #          unique() %>%
    #          sort()
    #      county_choice <- c("All Counties", counties)
    #      updateSelectInput(session, "county_input", choices = county_choice, selected = "All Counties")
    #  })
    
     # data <- reactive({
     #     #Can't get this reactive geometry to work...#
     #     req(input$geo_input)
     # if ("County" %in% input$geo_input){county_shp}
     #     else if ("ZCTA" %in% input$geo_input){zcta_shp}
     #     else if ("Census Tracts" %in% input$geo_input){tract_shp}
     # })
     # 
     # data <- reactive({
     #     req(input$geo_input)
     #     if("County" %in% input$geo_input) read_rds("data/county_2010_12_sf.rds") %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
     #     else if ("ZCTA" %in% input$geo_input) read_rds("data/zcta_2010_12_sf.rds") %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
     #     else if ("Census Tracts" %in% input$geo_input) read_rds("data/tract_2010_12_sf.rds")  %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
     # })
    
     # filtered_data <- reactive({
     #     req(input$geo_input, input$year_input, input$state_input)
     #      if ("All Counties" %in% input$county_input){
     #          data() %>%
     #      filter(state.abb %in% input$state_input & year %in% input$year_input)
     #      } else {
     #          req(input$county_input)
     #          data() %>%
     #         filter(state.abb %in% input$state_input & year %in% input$year_input & county.name %in% input$county_input) 
     #          
     #      }
     # })
     # 
     filtered_data <- reactive({
         req(input$geo_input, input$year_input, input$state_input)
             data() %>%
                 filter(state.abb %in% input$state_input & year %in% input$year_input) 
     })

    
    # center_lon <- reactive({
    #     filtered_data() %>%
    #         st_buffer(dist = 0.15) %>%
    #         st_bbox() %>%
    #         mean(c(.$xmin, .$xmax))
    # })
    # 
    # center_lat <- reactive({
    #     filtered_data() %>%
    #         st_buffer(dist = 0.15) %>%
    #         st_bbox() %>%
    #         mean(c(.$ymin, .$ymax))
    # })
    # 
    # 
    # 
    # 

    output$map <- renderLeaflet({
        pal = colorNumeric(
            palette = c("orange", "white", "blue"),
            domain = c(-1, 1))
        
        plot_mapbox() %>%
            add_sf(
                data = filtered_data(),
                split = ~GEOID10,
                fillcolor = ~pal(get(input$seg_input)),
                opacity = 0.7,
                hoverinfo = "text",
                text = ~paste(county.name, ": ", round(get(input$seg_input), 2)),
                showlegend = FALSE,
                stroke = FALSE
            ) %>%
            #I can't get the color scale to match.
             # add_trace(
             #     data = filtered_data(),
             #     type = "scattermapbox",
             #     mode = "markers",
             #     lat = center_lat(),
             #     lon = center_lon(),
             #     marker = list(
             #          size = 0,
             #          opacity = 0,
             #          cmin = -1,
             #          cmax = 1,
             #          color = ~pal(get(input$seg_input)),
             #          colorbar = list(
             #              title = "color bar"
             #         )
             #     )
             # ) %>%
             layout(
                 mapbox = list(
                     style = "mapbox://styles/mapbox/light-v10",  # Choose a mapbox style
                   #  center = list(lon = center_lon, lat = center_lat),  # Set center coordinates
                     zoom = 8  # Set zoom level
                 )
             ) 
    })


    
    
    
    }





# Run the application -----
shinyApp(ui = ui, server = server)

