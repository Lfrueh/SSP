library(shiny)
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(sf)
library(plotly)
library(leaflet)
library(arrow)
library(sfarrow)
library(leaflet.extras)

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
ui <- navbarPage(
  "Index of Concentration at the Extremes",
## Dashboard ----
  tabPanel("Dashboard", 
           sidebarLayout(
             sidebarPanel(
               HTML(paste("<h3> Select Data </h3>")),
               selectInput(inputId = "year_input",
                           label = "Select a Year",
                           choices = seq(2010, 2019, 1),
                           multiple = FALSE),
               selectInput(inputId = "state_input",
                           label = "Select a State",
                           choices = state_val,
                           selected = "PA",
                           multiple = FALSE),
               uiOutput("county_select"),
               selectInput(inputId = "seg_input",
                           label = "Select a Segregation Measure",
                           choices = ice_val,
                           multiple = FALSE),
               selectInput(inputId = "geo_input",
                           label = "Select Geography",
                           choices = c("County","ZCTA", "Census Tracts"),
                           multiple = FALSE,
                           selected = "County"),
               HTML(paste("<em> Note that ZCTA and Census Tract data may take longer to load. </em>")),
               HTML(paste("<h3> Download Data </h3>")),
               fluidRow(
                 column(width = 6,  # 50% width
                        selectInput(inputId = "download_type",
                                    label = NULL,
                                    choices = c("CSV", "Esri Shapefile", "GeoJSON"))),
                 column(width = 6,  # 50% width
                        HTML(""),
                        downloadButton("download", "Download"))
               )
             ),
             mainPanel(
               uiOutput("helptext"),
              leafletOutput("leafletmap"),
               plotlyOutput("histogram")
             )
           ),
           tags$style(
             HTML(".shiny-output-error {
      color: white; /* Set the text color to white */
    }")
           ),
           fluid = TRUE),

## Details and Methodology Tab ----
  tabPanel("Details and Methodology",
           includeHTML("details.Rhtml"),
           HTML(paste("<h3> Variable Definitions </h3>")),
           tableOutput("data_def"),
           HTML(paste("<h3> Data Availability </h3>")),
           tableOutput("data_avail"),
           HTML(paste("<h3> Using This Tool </h3>")),
           img(src='howto.png', width = "100%"),
           fluid = TRUE)
)


# Server -----
pal = colorNumeric(
  palette = colorRamp(c("orange", "white", "blue")),
  domain = c(-1, 1))


server <- function(input, output, session) {
  
  # Reactive Expressions ----
  
  #THe joined reactive datasets are, for some reason, data.frame class and not sf class.
  county_data <- reactive({
    data <- open_dataset("data/county_partitioned") %>%
      filter(year %in% !!input$year_input, state.abb %in% !!input$state_input) %>%
      collect()
    
    data_sf <- open_dataset("data/county_sf_partitioned") %>%
      filter(state.abb %in% input$state_input)  %>%
      read_sf_dataset(., find_geom = TRUE)
    
    data_joined <- left_join(data, data_sf, by=c("GEOID10", "state.abb", "county.name"))
    return(st_as_sf(data_joined))
  }) 
  
  zcta_data <- reactive({
    data <- open_dataset("data/zcta_partitioned") %>%
      filter(year %in% !!input$year_input, state.abb %in% !!input$state_input) %>%
      collect()
    
    data_sf <- open_dataset("data/zcta_sf_partitioned") %>%
      filter(state.abb %in% !!input$state_input)  %>%
      read_sf_dataset(., find_geom = TRUE)
    
    data_joined <- left_join(data, data_sf, by=c("GEOID10", "state.abb", "county.name"))
    return(st_as_sf(data_joined))
  }) 

  tract_data <- reactive({
    data <- open_dataset("data/tract_partitioned") %>%
      filter(year %in% !!input$year_input, state.abb %in% !!input$state_input) %>%
      collect()
    
    data_sf <- open_dataset("data/tract_sf_partitioned") %>%
      filter(state.abb %in% !!input$state_input)  %>%
      read_sf_dataset(., find_geom = TRUE)
    
    data_joined <- left_join(data, data_sf, by=c("GEOID10", "state.abb", "county.name"))
    return(st_as_sf(data_joined))
  }) 

  
  ## County Choices ----
  county_choices <- reactive({
    county_data() %>% 
      pull(county.name) %>%
      unique() %>%
      sort()
  }) 

  
  ### Update county selection UI ----
  output$county_select <- renderUI({
    selectInput(inputId = "county_input",
                label = "Select Counties",
                choices = county_choices(),
                multiple = TRUE)
  })
  
  ## Filtered data ----
  data2 <- reactive({
    if(input$geo_input=="County"){
      data <- county_data()
    }
    else if(input$geo_input=="ZCTA"){
      data <- zcta_data()  
    } else if(input$geo_input=="Census Tracts"){
      data <- tract_data() 
    }
    return(data)
  }) 
  
   data <- reactive({
     data <- data2() %>% filter(
         is.null(input$county_input) | county.name %in% input$county_input)
     return(data)
   }) 
  
  
  
  
  ## Help Text ----
   helptext <- reactive({
     if (is.null(data()) || nrow(data()) == 0) {
         return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
           ZCTA Available in years 2011 and later.
                       </p>"))

     }
       
     else if (input$seg_input == "ICEedu" && input$year_input < 2012){
       return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
           ICE Education available in years 2012 and later.
                       </p>"))  
     }
     else {
       return(NULL)
     }
   })
   
   output$helptext <- renderUI({
     helptext()
   })
  
  
  ## Hover Text -----
  hovertext <- reactive({
    ice_name <- names(ice_val[ice_val == input$seg_input])
    if (input$geo_input == "County"){
      ~paste0('<b>',county.name, "</b><br>", ice_name, ': <b>',round(get(input$seg_input), 2),'</b>')
    } else if (input$geo_input == "ZCTA"){
      ~paste0('<b>',GEOID10, "</b><br>", ice_name, ': <b>',round(get(input$seg_input), 2),'</b>')
    } else if (input$geo_input == "Census Tracts"){
      ~paste0('<b>',tract.name,"</b><br>", county.name,'<br>',ice_name, ': <b>', round(get(input$seg_input), 2),'</b>')
    }
  })
   
   ## Histogram Y axis label -----
   y_label <- reactive({
     if (input$geo_input == "County") {
    text <- "N Counties"
   } else if (input$geo_input == "ZCTA") {
    text <- "N ZCTAs"
   } else if (input$geo_input == "Census Tracts"){
    text <-  "N Tracts"
   }
     text
   })
  
  
  # Outputs ----- 
   ## Histogram -----
   
   output$histogram <- renderPlotly({
     ice_name <- names(ice_val[ice_val == input$seg_input])
     
     hist <- data() %>%
       filter(!is.na(get(input$seg_input))) %>%
       ggplot(aes(x = get(input$seg_input), fill = ..x..)) +
       geom_histogram(bins = 50, col = I("grey")) +
       scale_fill_gradient2(low='orange', mid='white', high='blue',  limits = c(-1,1),
                            name = input$seg_input) +
       labs(
         x = ice_name,
         y = y_label()
       ) + 
       theme_minimal() + 
       theme(legend.position="none")
     
     hist %>% ggplotly(source="histogram", tooltip = NULL) %>% 
       layout(dragmode = "select",
               modebar = list(
                 orientation = "v",
                remove = c("lasso","pan","autoscale", "zoomin", "zoomout",
                           "toImage", "hoverCompareCartesian", "toggleHover", "hoverClosestCartesian"))) %>%
       config(displaylogo=FALSE,
              displayModeBar=TRUE) %>%
       event_register("plotly_selected")
   })



   ## Leaflet Map ----
   
   output$leafletmap <- renderLeaflet({
     ice_name <- names(ice_val[ice_val == input$seg_input])
     d <- event_data("plotly_selected", source = "histogram")
     selected_range <- if (!is.null(d)) {
       c(min(d$x), max(d$x))} else{
         NULL
       }
     
      if (is.null(selected_range)){
        data <- data()
      } else{
        data <- data() %>%
          filter(get(input$seg_input) >= selected_range[1] & get(input$seg_input) <= selected_range[2])
      }
     
     data %>%
       leaflet() %>% 
       addMapPane(name = "polygons", zIndex = 410) %>% 
       addMapPane(name = "maplabels", zIndex = 420) %>%
       addProviderTiles("CartoDB.PositronNoLabels") %>%
       addProviderTiles("CartoDB.PositronOnlyLabels", 
                        options = leafletOptions(pane = "maplabels"),
                        group = "Map Labels") %>%
       addLegend(pal = pal, values = data()[[input$seg_input]], opacity = 1, title = ice_name) %>%
       addPolygons(color = "gray", weight = 0.7, fillOpacity = 0.9, fillColor= ~pal(get(input$seg_input)),
                   popup = hovertext(),
                   group = ice_name)  %>%
       addLayersControl(overlayGroups = c("Map Labels",
                                          ice_name))
   })

   

   


   
  
  ## Download Button ----
   output$download <- downloadHandler(
     filename = function() {
       geo <- input$geo_input
       state <- input$state_input
       county <- if (is.null(input$county_input) || length(input$county_input) == 0) {
         "All_Counties"  # Default value if no county selected
       } else {
         paste(input$county_input, collapse = "_")
       }
       year <- input$year_input
       suffix <- if (input$download_type == "CSV"){
         ".csv"
       } else if (input$download_type == "Esri Shapefile"){
         ".shp"
       } else if (input$download_type == "GeoJSON"){
         ".geojson"
       }
       # Set the file name for the downloaded file
       paste(geo, state, county, year, suffix, sep = "_")
     },
     content = function(file){
       data <- if (input$download_type == "CSV"){
         data() %>% st_drop_geometry(.)
       } else if (input$download_type == "Esri Shapefile") {
         data() %>% rename_all(~substr(., 1, 10))
       } else if (input$download_type == "GeoJSON"){
         data()
       }
       if (input$download_type == "CSV"){
         write.csv(data, file)
       } else if (input$download_type == "Esri Shapefile"){
         st_write(data, dsn=file, driver = "Esri Shapefile")
       } else if (input$download_type == "GeoJSON"){
         st_write(data, dsn=file, driver = "GeoJSON")
       }
     }
   )
   

  
  ## Methodology ---- 
  output$data_def <- renderTable(
    read_excel("data/data_def.xlsx")
  )
  output$data_avail <- renderTable(
    read_excel("data/data_avail.xlsx")
  )
  
  
}



# Run the application -----
shinyApp(ui = ui, server = server)

