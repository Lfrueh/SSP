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
data <- county_shp %>% filter(year == 2010 & state.abb == "PA") %>% st_make_valid()

bbox <- 
  data %>%
  st_buffer(dist = 0.15) %>%
  st_bbox() 

center_lat <- mean(c(bbox$xmin, bbox$xmax))
center_lon <- mean(c(bbox$ymin, bbox$ymax))

  pal = colorNumeric(
    palette = c("orange", "white", "blue"),
    domain = c(-1, 1))
  
  plot_mapbox(
    data = data,
    split = ~GEOID10,
    fillcolor = ~pal(ICEincome),
    opacity = 0.5,
    hoverinfo = "text",
    text = ~paste(county.name, ": ", round(ICEincome, 2)),
    showlegend = FALSE,
    stroke = FALSE
  ) %>%
  layout(
    mapbox = list(
      style = "mapbox://styles/mapbox/light-v10",  # Choose a mapbox style
      center = list(lon = center_lon, lat = center_lat),  # Set center coordinates
      zoom = 8  # Set zoom level
    )
  ) 