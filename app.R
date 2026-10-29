
{
#Dependencies
library(shiny)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
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

# CARTO basemap tiles (a free API key is now required) -----
# Set CARTO_API_KEY in .Renviron (see README). Request a key at
# https://carto.com/basemaps/apikey/
carto_key <- Sys.getenv("CARTO_API_KEY")
if (!nzchar(carto_key)) {
  warning("CARTO_API_KEY is not set; basemap tiles will be watermarked.")
}
carto_attribution <- paste0(
  "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors, ",
  "&copy; <a href='https://carto.com/attributions'>CARTO</a>"
)
carto_tiles <- function(style) {
  paste0("https://basemaps.cartocdn.com/rastertiles/", style,
         "/{z}/{x}/{y}.png?key=", carto_key)
}

# Create selection options -----
# DC and Puerto Rico are in the county and ZCTA data. Combinations without data
# (e.g. tracts in Puerto Rico) show a message instead of an empty map.
state_val <- sort(c(state.abb, "DC", "PR"))

# Read once per R process: these two small tables do not change between
# sessions, so there is no reason to re-read the Excel files for every user.
data_def_table <- read_excel("data/data_def.xlsx")
data_avail_table <- read_excel("data/data_avail.xlsx")

ice_val <- c(
  "ICE Income" = "ICEincome",
  "ICE Race/ethnicity" = "ICEraceeth",
  "ICE Homeownership" = "ICEhome",
  "ICE Income + Race" = "ICEincwb",
  "ICE Income + Race/ethnicity" = "ICEincwnh",
  "ICE Education" = "ICEedu",
  "ICE Language" = "ICElanguage"
)

# Open the parquet datasets once per R process, not inside each reactive.
# Opening is lazy (it only reads file listings and schemas), so every session
# shares these objects and the server functions only pay for the rows they filter.
open_geo_datasets <- function(prefix) {
  list(
    attrs = open_dataset(paste0("data/", prefix, "_partitioned")),
    geom = open_dataset(paste0("data/", prefix, "_sf_partitioned"))
  )
}

geo_datasets <- list(
  "County" = open_geo_datasets("county"),
  "ZCTA" = open_geo_datasets("zcta"),
  "Census Tracts" = open_geo_datasets("tract")
)

# Read one state's boundaries. sfarrow::read_sf_dataset() ignores the state
# filter with arrow >= ~13 and reads the whole country every time, so filter and
# collect with arrow, then rebuild the sf object from the WKB geometry column.
# Boundaries were saved in EPSG:4326 by build_sf_data.R.
read_state_geom <- function(dataset, state) {
  geom <- dataset |>
    filter(state.abb == !!state) |>
    collect()

  st_sf(
    select(geom, -geometry),
    geometry = st_as_sfc(
      structure(lapply(geom$geometry, as.raw), class = "WKB"),
      crs = 4326
    )
  )
}

# Popup text per area. Built as plain strings in R and sent to the browser,
# because the polygons are drawn once and only restyled afterwards.
popup_html <- function(df, geo, seg, ice_name) {
  value <- round(df[[seg]], 2)
  title <- switch(
    geo,
    "County" = paste0("<b>", df$county.name, "</b>"),
    "ZCTA" = paste0("<b>", df$GEOID10, "</b>"),
    "Census Tracts" = paste0("<b>", df$tract.name, "</b><br>", df$county.name)
  )
  paste0(title, "<br>", ice_name, ": <b>", value, "</b>")
}

# User interface -----
ui <- fluidPage(
  useWaiter(),
  waiterShowOnLoad(html = loading_screen,
                   color = 'white'),
  ## Loader code: UI (end)
  tags$head(includeCSS("CSS/Header.css")),
  tags$head(includeCSS("CSS/NavbarPage.css")),
  tags$head(includeCSS("CSS/Home.css")),
  tags$head(tags$script(src = "ssp-map.js")),
  tags$head(tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico")),
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
  # The loading spinner is hidden by the map observer once the first draw is done.

  # Reactive Expressions ----

  ## Geometry ----
  # Boundaries depend only on geography and state, not on year or measure, so
  # they are read once per combination and cached instead of re-read on every
  # year change.
  geom_data <- reactive({
    read_state_geom(geo_datasets[[input$geo_input]]$geom, input$state_input)
  }) |>
    bindCache(input$geo_input, input$state_input)

  ## Attributes ----
  # Only the ICE values change with year; they are small compared to geometry.
  # state.abb and county.name already come with the geometry, so drop them here
  # to keep the join free of duplicate columns.
  attr_data <- reactive({
    state <- input$state_input
    year <- as.integer(input$year_input)
    geo_datasets[[input$geo_input]]$attrs |>
      filter(year == !!year, state.abb == !!state) |>
      select(-any_of(c("state.abb", "county.name"))) |>
      collect()
  }) |>
    bindCache(input$geo_input, input$state_input, input$year_input)

  ## County Choices ----
  # Taken from the geometry so the list does not depend on year and the county
  # selection is not reset when only the year changes.
  county_choices <- reactive({
    geom_data()$county.name |>
      unique() |>
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
  # Keeping the sf object on the left of the join keeps the sf class. The inner
  # join keeps only areas that have both a boundary and data for the chosen year.
  data2 <- reactive({
    inner_join(
      geom_data(),
      attr_data(),
      by = join_by(GEOID10),
      relationship = "one-to-one"
    )
  })
  
   data <- reactive({
     if (is.null(input$county_input)) {
       data <- data2()
     } else {
       data <- data2() %>% filter(
       county.name %in% input$county_input
       )
     }

     return(data)
   }) 
  
  
  
  
  ## Help Text ----
    # The year input is text, so convert it before comparing.
    helptext <- reactive({
      year <- as.integer(input$year_input)
      message <- if (input$geo_input == "ZCTA" && year == 2010) {
        "ZCTA Available in years 2011 and later."
      } else if (input$seg_input == "ICEedu" && year < 2012) {
        "ICE Education available in years 2012 and later."
      } else if (nrow(data()) == 0) {
        # Catches any other gap in the data (e.g. tracts in Puerto Rico).
        "No data are available for this combination of year, state and geography."
      }

      if (is.null(message)) {
        return(NULL)
      }
      tags$p(
        style = "font-size: 16px; font-weight: bold; color: red;",
        message
      )
    })
    
    output$helptext <- renderUI({
      helptext()
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
     # The reason is shown in the help text above the map.
     validate(need(nrow(data()) > 0, "No data to plot for this selection."))
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
   
   ### Base map ----
   # Built once. Tiles never change, so they stay out of the observers below,
   # which only redraw polygons through leafletProxy().
   output$leafletmap <- renderLeaflet({
     leaflet() |>
       addMapPane(name = "polygons", zIndex = 410) |>
       addMapPane(name = "maplabels", zIndex = 420) |>
       addTiles(urlTemplate = carto_tiles("light_nolabels"),
                attribution = carto_attribution) |>
       addTiles(urlTemplate = carto_tiles("light_only_labels"),
                attribution = carto_attribution,
                options = tileOptions(pane = "maplabels"),
                group = "Map Labels") |>
       setView(lng = -96, lat = 38, zoom = 4) |>
       addLayersControl(overlayGroups = c("Map Labels", "ICE values"))
   })

   # leaflet drops proxy calls sent before the browser has built the map, and
   # sets its zoom input as soon as it has, so wait for that first.
   map_ready <- reactiveVal(FALSE)
   observeEvent(input$leafletmap_zoom, map_ready(TRUE), once = TRUE)

   ### Area on the map ----
   # Boundaries for the selected state, narrowed to the chosen counties. This
   # does not depend on year, measure or brush, so those never redraw polygons.
   area_geom <- reactive({
     area <- geom_data()
     if (!is.null(input$county_input)) {
       area <- filter(area, county.name %in% input$county_input)
     }
     area
   })

   ### Draw polygons ----
   # Sent once per area. They start invisible and get their colors from the
   # restyle observer below. layerId lets the browser match styles to polygons.
   shapes_version <- reactiveVal(0)
   observe({
     req(map_ready())
     area <- area_geom()

     proxy <- leafletProxy("leafletmap") |>
       clearShapes()

     if (nrow(area) > 0) {
       proxy |>
         addPolygons(
           data = area,
           layerId = ~GEOID10,
           group = "ICE values",
           color = "gray",
           weight = 0.7,
           opacity = 0,
           fillOpacity = 0
         )
     }

     # Tells the restyle observer the polygons it should style are now drawn.
     shapes_version(isolate(shapes_version()) + 1)
   })

   ### Histogram brush ----
   # NULL when nothing is brushed.
   brush_range <- reactive({
     d <- event_data("plotly_selected", source = "histogram")
     if (is.null(d)) NULL else range(d$x)
   })

   ### Color polygons ----
   # Year, measure and brush only change colors, popup text and dimming, so send
   # just those. Areas outside the brush are faded, not removed, so the map keeps
   # its geographic context.
   observe({
     req(map_ready(), shapes_version() > 0)
     seg <- input$seg_input
     ice_name <- names(ice_val[ice_val == seg])
     df <- st_drop_geometry(data())
     value <- df[[seg]]

     brush <- brush_range()
     dim_area <- if (is.null(brush)) {
       rep(FALSE, nrow(df))
     } else {
       !(value >= brush[1] & value <= brush[2]) | is.na(value)
     }

     # Areas with a missing value stay on the map in the palette's NA gray.
     fill <- pal(value)
     fill[is.na(fill)] <- "#808080"

     restyle <- list(
       id = "leafletmap",
       ids = df$GEOID10,
       fill = fill,
       popup = popup_html(df, input$geo_input, seg, ice_name),
       dim = dim_area
     )

     # leafletProxy() holds its calls until the end of the flush, so hold this
     # message too, otherwise it reaches the browser before the polygons do.
     session$onFlushed(
       function() session$sendCustomMessage("ssp-restyle", restyle),
       once = TRUE
     )

     proxy <- leafletProxy("leafletmap")
     # Nothing to show (e.g. ZCTA in 2010): drop the old legend so it is not stale.
     if (nrow(df) == 0) {
       removeControl(proxy, "ice_legend")
     } else {
       addLegend(
         proxy,
         pal = pal,
         values = value,
         opacity = 1,
         title = ice_name,
         layerId = "ice_legend"
       )
     }

     # First colors are on the map, so it is ready to look at.
     waiter_hide()
   })

   ### Zoom to the selection ----
   # Refit only when the area changes (state, geography, counties), not when
   # year or measure change, so the view does not jump while exploring.
   view_bounds <- reactive({
     st_bbox(area_geom())
   })

   observe({
     req(map_ready())
     bounds <- view_bounds()
     req(all(is.finite(bounds)))
     leafletProxy("leafletmap") |>
       fitBounds(
         lng1 = bounds[["xmin"]],
         lat1 = bounds[["ymin"]],
         lng2 = bounds[["xmax"]],
         lat2 = bounds[["ymax"]]
       )
   })

   

   


   
  
  ## Download Button ----
   # The download holds every area in the current selection (year, state,
   # geography, counties), the same areas the map shows. The histogram brush
   # only fades areas on the map and does not change the download.
   download_base_name <- reactive({
     county <- input$county_input
     county_label <- if (length(county) == 0) {
       "All_Counties"
     } else if (length(county) == 1) {
       county
     } else {
       # Many county names would make an unusably long file name.
       paste0(length(county), "_Counties")
     }

     name <- paste(
       input$geo_input, input$state_input, county_label, input$year_input,
       sep = "_"
     )
     gsub("[^A-Za-z0-9_-]+", "_", name)
   })

   output$download <- downloadHandler(
     filename = function() {
       extension <- switch(
         input$download_type,
         "CSV" = "csv",
         # A shapefile is several files, so it is delivered as a zip.
         "Esri Shapefile" = "zip",
         "GeoJSON" = "geojson"
       )
       paste0(download_base_name(), ".", extension)
     },
     content = function(file) {
       download_data <- data()

       if (input$download_type == "CSV") {
         write.csv(st_drop_geometry(download_data), file, row.names = FALSE)
       } else if (input$download_type == "GeoJSON") {
         st_write(
           download_data, dsn = file, driver = "GeoJSON",
           delete_dsn = TRUE, quiet = TRUE
         )
       } else if (input$download_type == "Esri Shapefile") {
         # Shapefile column names are limited to 10 characters.
         shp_data <- rename_all(download_data, ~ substr(., 1, 10))
         shp_dir <- tempfile("shapefile")
         dir.create(shp_dir)
         on.exit(unlink(shp_dir, recursive = TRUE), add = TRUE)

         st_write(
           shp_data,
           dsn = file.path(shp_dir, paste0(download_base_name(), ".shp")),
           driver = "ESRI Shapefile", quiet = TRUE
         )
         zip::zipr(file, files = list.files(shp_dir, full.names = TRUE))
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
  output$data_def <- renderTable(data_def_table)
  output$data_avail <- renderTable(data_avail_table)
  
  
}



# Run the application -----
shinyApp(ui = ui, server = server)

