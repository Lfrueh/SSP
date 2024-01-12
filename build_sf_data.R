library(sf)
library(readr)
library(tigris)
library(rmapshaper)
library(tidyverse)
library(arrow)
library(sfarrow)

# State names and crosswalks ----
fips <- read_csv(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
) %>%
  select(-classfp)

zcta_to_county <- read_delim(
  "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt",
  delim = ","
) %>% 
  rename(
    GEOID10 = ZCTA5,
    statefp = STATE,
    countyfp = COUNTY
  ) 

# Shapefiles -----
# First, read in shapefiles for 2010 census tract, ZCTA, and county boundaries
## Counties ----
county_sf <- counties(year = 2010, state = NULL) %>% 
  st_transform(crs = 'epsg:4326') %>% 
  ms_simplify(., keep = 0.2, keep_shapes = TRUE, sys = TRUE) %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.) %>%
  rename(statefp = STATEFP10, countyfp = COUNTYFP10) %>%
  left_join(., fips, by=c("statefp", "countyfp")) %>%
  select(GEOID10, state.abb, county.name)

## ZCTA ----
zcta_sf <- zctas(year = 2010, state = NULL) %>% 
  st_transform(crs = 'epsg:4326') %>% 
  ms_simplify(., keep = 0.2, keep_shapes = TRUE, sys = TRUE) %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.) %>%
  select(GEOID10) %>%
  left_join(., zcta_to_county, by = "GEOID10", relationship = "many-to-many") %>%
  left_join(., fips, by = c("statefp", "countyfp")) %>%
  select(GEOID10, state.abb, county.name)

## Tracts ----

most_states <- state.name[2:50]

# Initialize an empty sf frame to store state data
all_tracts <- tracts(year = 2010, state = "Alabama")

# Loop through each state and fetch the census tract shapefile
for (state in most_states) {
  state_tract <- tracts(year = 2010, state = state)
  all_tracts <- rbind(state_tract, all_tracts)
}

tract_sf <- all_tracts %>% 
  st_transform(crs = 'epsg:4326') %>% 
  ms_simplify(., keep = 0.2, keep_shapes = TRUE, sys = TRUE) %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)

tract_sf <- tract_sf %>%
  rename(statefp = STATEFP10, countyfp = COUNTYFP10, tract.name = NAMELSAD10) %>%
  left_join(., fips, by=c("statefp", "countyfp")) %>%
  select(GEOID10, state.abb, county.name, tract.name)


#Download year/ice tables
data_years <- seq(2010, 2019, 1)

# Non-Spatial Data ----

## County -----
data_county <- data.frame()
for (year in data_years){
  data <- read_csv(paste0("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_",
                          year,"_county.csv")) 
  data$year <- year
  data_county <- bind_rows(data_county, data)
}

data_county <- data_county %>%
  rename(GEOID10 = GEOID) %>%
  mutate(statefp = str_sub(GEOID10, 1, 2),
         countyfp = str_sub(GEOID10, 3, 5)) %>%
  left_join(., fips, by=c("statefp", "countyfp"))

## ZCTA -----
data_zcta <- data.frame()
for (year in data_years[2:10]){
  data <- read_csv(paste0("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_",
                          year,"_zcta.csv")) 
  data$year <- year
  data_zcta <- bind_rows(data_zcta, data)
}

data_zcta <- data_zcta %>%
  rename(GEOID10 = GEOID) %>%
  left_join(., zcta_to_county, by = "GEOID10", relationship = "many-to-many") %>%
  left_join(., fips, by = c("statefp", "countyfp"))

## Tracts -----
data_tract <- data.frame()
for (year in data_years){
  data <- read_csv(paste0("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_",
                          year,"_tract.csv")) 
  data$year <- year
  data_tract <- bind_rows(data_tract, data)
}

data_tract <- data_tract %>%
  rename(GEOID10 = GEOID) %>%
  mutate(statefp = str_sub(GEOID10, 1, 2),
         countyfp = str_sub(GEOID10, 3, 5)) %>%
  left_join(., fips, by=c("statefp", "countyfp"))


# Save objects as arrow datasets ----
## Non-spatial ---
write_dataset(data_county, "data/county_partitioned",
              partitioning = c("year", "state.abb"))
write_dataset(data_zcta, "data/zcta_partitioned",
              partitioning = c("year", "state.abb"))
write_dataset(data_tract, "data/tract_partitioned",
              partitioning = c("year", "state.abb"))

## Spatial ----
sfarrow::write_sf_dataset(county_sf,"data/county_sf_partitioned", partitioning=c("state.abb"))
sfarrow::write_sf_dataset(zcta_sf,"data/zcta_sf_partitioned", partitioning=c("state.abb"))
sfarrow::write_sf_dataset(tract_sf,"data/tract_sf_partitioned", partitioning=c("state.abb"))

