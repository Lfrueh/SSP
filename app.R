library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(sf)
library(plotly)
library(mapboxapi)
library(leaflet)
library(shinycssloaders)
library(readxl)

mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
mb_access_token(mapbox_token)



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
  tabPanel("Interactive Map", 
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
               downloadButton("download", "Download Filtered Data as .csv")
             ),
             # Show map
             mainPanel(
               # Conditional help text
               uiOutput("helptext"),
               withSpinner(
                 plotlyOutput("map")
               ),
               plotlyOutput("histogram")
             )
           ),
           tags$style(
             HTML(".shiny-output-error {
      color: white; /* Set the text color to white */
    }")
           ),
           fluid = TRUE),
  tabPanel("Details and Methodology",
           includeHTML("details.Rhtml"),
           HTML(paste("<h3> Variable Definitions </h3>")),
           tableOutput("data_def"),
           HTML(paste("<h3> Data Availability </h3>")),
           tableOutput("data_avail"),
           fluid = TRUE)
)


# Server -----
pal = colorNumeric(
  palette = colorRamp(c("orange", "white", "blue")),
  domain = c(-1, 1))


server <- function(input, output, session) {
  
  # Reactive Expressions ----
  
  county_data <- reactive({
    data <- read_rds("data/county_2010_19_sf.rds") %>% filter(year == input$year_input)
    return(data)
  }) 

  zcta_data <- reactive({
    data <- read_rds("data/zcta_2010_19_sf.rds") %>% filter(year == input$year_input)
    return(data)
  }) 

  tract_data <- reactive({
    data <- read_rds("data/tract_2010_19_sf.rds") %>% filter(year == input$year_input)
    return(data)
  }) 

  
  ## County Choices ----
  county_choices <- reactive({
    county_data() %>% 
      filter(state.abb == input$state_input) %>%
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
      imported_data <- county_data()
    }
    else if(input$geo_input=="ZCTA"){
      imported_data <- zcta_data()  
    } else if(input$geo_input=="Census Tracts"){
      imported_data <- tract_data() 
    }
    return(imported_data)
  }) 
  
  data <- reactive({
    data <- data2() %>% filter(
      state.abb == input$state_input &
        (is.null(input$county_input) | county.name %in% input$county_input))
    return(data)
  }) 
  
  
  
  
  ## Help Text ----
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
    if (input$geo_input == "County"){
      ~paste(county.name, ": ", round(get(input$seg_input), 2))
    } else if (input$geo_input == "ZCTA"){
      ~paste(GEOID10, ": ", round(get(input$seg_input), 2))
    } else if (input$geo_input == "Census Tracts"){
      ~paste(NAMELSAD10, ": ", round(get(input$seg_input), 2))
    }
  })
  
  
  # Outputs ----- 
  ## Histogram -----
  output$histogram <- renderPlotly({
    data() %>%
      filter(!is.na(get(input$seg_input))) %>%
      ggplot(aes(x = get(input$seg_input), fill = ..x..)) +
      geom_histogram(bins = 50, col = I("grey")) +
      scale_fill_gradient2(low='orange', mid='white', high='blue',  limits = c(-1,1),
                           name = input$seg_input) +
      labs(
        x = input$seg_input,
        y = "Count"
      ) + 
      theme_minimal()
  })
  
  
  ## Map -----
  output$map <- renderPlotly({
    ice_name <- names(ice_val[ice_val == input$seg_input])
    plot_mapbox(
      data = data(),
      split = ~GEOID10,
      fillcolor = ~pal(get(input$seg_input)),
      opacity = 0.5,
      hoverinfo = "text",
      text = hovertext(),
      showlegend = FALSE,
      stroke = I("grey")) %>%
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
  
  ## Download Button ----
  output$download <- downloadHandler(
    filename = function() {
      ice <- input$seg_input
      geo <- input$geo_input
      state <- input$state_input
      county <- if (is.null(input$county_input) || length(input$county_input) == 0) {
        "All_Counties"  # Default value if no county selected
      } else {
        paste(input$county_input, collapse = "_")
      }
      year <- input$year_input
      # Set the filename for the downloaded CSV
      paste(ice, geo, state, county, year, ".csv", sep = "-")
    },
    content = function(file) {
      data_dl <- data() %>%
        st_drop_geometry(.)
      # Write the filtered data to a CSV file
      write.csv(data_dl, file)
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

