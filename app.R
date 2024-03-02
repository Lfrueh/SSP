
{
#Dependencies
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
library(waiter)

  ## Loader code: Global Scope
  loading_screen = div(
    tags$img(
      src = "logo.png",
      height = 175
    ),
    div(
      style = "padding-top: 50px;",
      spin_loader()) )
  
  
}

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
  useWaiter(),
  waiterShowOnLoad(html = loading_screen,
                   color = 'white'),
  ## Loader code: UI (end)
  tags$head(includeCSS("CSS/Header.css")),
  tags$head(includeCSS("CSS/NavbarPage.css")),
  tags$head(includeCSS("CSS/Home.css")),
  tags$head(includeHTML("HTML/FontAwesomeLoader.html")),
  includeHTML("HTML/Header.html"),
  navbarPage(
  "Index of Concentration at the Extremes",
## Dashboard ----
  tabPanel("Dashboard", 
           HTML("<h2>Spatial Social Polarization (SSP) Maps:<br>
                The Index of Concentration at the Extremes (ICE)</h2>"),
           tags$head(
               includeHTML("HTML/google-analytics.Rhtml")
           ),
           sidebarLayout(
             sidebarPanel(
               HTML(paste("<h3> Select Data </h3>")),
                selectInput(inputId = "year_input",
                            label = "Select a Year",
                            choices = seq(2010, 2019, 1),
                            selected = 2019,
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
              HTML(paste('<p></p>')),
              uiOutput("desc_text"),
               plotlyOutput("histogram")
             )
           ),
           tags$style(
             HTML(".shiny-output-error {
      color: white; /* Set the text color to white */
    }")
           ),
           fluid = TRUE),

## Using this Tool Tab ----

  tabPanel("Using this Tool",
           fluidRow(
           HTML(paste("<h2> Using This Tool </h2>")),
           img(src='howto.png', width = "100%"),
           width = 12)
  ),

## Details and Methodology Tab ----
  tabPanel("Details and Methodology",
           includeHTML("HTML/details.Rhtml"),
           HTML(paste("<h3> Variable Definitions </h3>")),
           tableOutput("data_def"),
           HTML(paste("<h3> Data Availability </h3>")),
           tableOutput("data_avail"),
           includeHTML("HTML/references.Rhtml"),
           fluid = TRUE)
),
includeHTML("HTML/Footer.html")
)


# Server -----
pal = colorNumeric(
  palette = colorRamp(c("orange", "white", "blue")),
  domain = c(-1, 1))


server <- function(input, output, session) {
  ## Loader code: Server (start)
  Sys.sleep(1) # do something that takes time
  waiter_hide()
  ## Loader code: Server (end)
  
  # Reactive Expressions ----
  
  #The joined reactive datasets are, for some reason, data.frame class and not sf class.
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
      if (input$geo_input == "ZCTA" & input$year_input == 2010){
        return(HTML("<p style='font-size: 16px; font-weight: bold; color: red;'>
            ZCTA Available in years 2011 and later.
                        </p>"))
      } else if (input$seg_input == "ICEedu" && input$year_input < 2012){
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
  
  ## Descriptive Text ----
    disadvantaged <- reactive({
      seg <- input$seg_input
      case_when(
        seg == "ICEincome" ~ "low-income people",
        seg == "ICEedu" ~ "people with less than a high school degree",
        seg == "ICEraceeth" ~ "Black non-Hispanic people",
        seg == "ICEhome" ~ "renter-occupied housing units",
        seg == "ICEincwb" ~ "Black low-income people",
        seg == "ICEincwnh" ~ "low-income people of color",
        seg == "ICElanguage" ~ "Spanish or Spanish Creole speakers",
        TRUE ~ "error"
      )
    })
  
    privileged <- reactive({
      seg <- input$seg_input
      case_when(
        seg == "ICEincome" ~ "high-income people",
        seg == "ICEedu" ~ "people with a college education",
        seg == "ICEraceeth" ~ "White non-Hispanic people",
        seg == "ICEhome" ~ "owner-occupied housing units",
        seg == "ICEincwb" ~ "White high-income people",
        seg == "ICEincwnh" ~ "high-income non-Hispanic White people",
        seg == "ICElanguage" ~ "English speakers",
        TRUE ~ "error"
      )
    })
    
  # Outputs ----- 
   ## Histogram -----
   
   output$histogram <- renderPlotly({
     ice_name <- names(ice_val[ice_val == input$seg_input])
     
     hist <- data() %>%
       filter(!is.na(get(input$seg_input)), get(input$seg_input)>=-1) %>%
       ggplot(aes(x = get(input$seg_input), fill = after_stat(x))) +
       geom_histogram(bins = 50, col = I("grey"), boundary = 0) +
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
                           "toImage", "hoverCompareCartesian", "toggleHover", "hoverClosestCartesian"))
                ) %>%
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
    
  ## Descriptive text ----
    output$desc_text <- renderUI({
      ice_name <- names(ice_val[ice_val == input$seg_input])
      HTML(paste0('<div style="border: 1px solid #e8e8e8; padding: 10px; background-color: #f5f5f5;border-radius:5px;">
                  <h3>',ice_name,'</h3>',
                 '<div style="display: flex; justify-content: space-between;">',
                 '<div style="width: 50%; text-align: left; color: orange; font-weight:bold;">
                 Negative values indicate a higher concentration of ', disadvantaged(), '.</div>',
                 '<div style="width: 50%; text-align: right; color: blue; font-weight:bold;">
                 Positive values indicate a higher concentration of ', privileged(), '.</div>',
                 '</div>',
                 '</div>'))
    })
   

  
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

