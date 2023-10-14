library(sf)
library(readr)
library(tigris)
library(rmapshaper)
library(tidyverse)

# Shapefiles -----
# First, read in shapefiles for 2010 census tract, ZCTA, and county boundaries
## Counties ----
county_2010 <- counties(year = 2010, state = NULL) %>% 
  st_transform(crs = 'epsg:4326') %>% 
  ms_simplify(., keep = 0.05, keep_shapes = TRUE, sys = TRUE)

## ZCTA ----
zcta_2010 <- zctas(year = 2010, state = NULL) %>% 
  st_transform(crs = 'epsg:4326') %>% 
  ms_simplify(., keep = 0.05, keep_shapes = TRUE, sys = TRUE)


## Tracts ----

most_states <- state.name[2:50]

# Initialize an empty sf frame to store state data
all_tracts <- tracts(year = 2010, state = "Alabama")

# Loop through each state and fetch the census tract shapefile
for (state in most_states) {
  state_tract <- tracts(year = 2010, state = state)
  all_tracts <- rbind(state_tract, all_tracts)
}

tract_2010 <- all_tracts %>% 
  st_transform(crs = 'epsg:4326') %>% 
  ms_simplify(., keep = 0.05, keep_shapes = TRUE, sys = TRUE)

# Non-spatial data ----
fips <- read_csv(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp")
) %>%
  select(-classfp)

#Download year/ice tables
data_years <- seq(2010, 2019, 1)

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
zcta_to_county <- read_delim(
  "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt",
  delim = ","
) %>% 
  rename(
    GEOID10 = ZCTA5,
    statefp = STATE,
    countyfp = COUNTY
  ) 

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


# Join with sf objects ----

county_joined <- left_join(county_2010, data_county, by = "GEOID10")  %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
zcta_joined <- left_join(zcta_2010, data_zcta, by = "GEOID10") %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)
tract_joined <- left_join(tract_2010, data_tract, by = "GEOID10")  %>% st_cast(., "MULTIPOLYGON") %>% st_make_valid(.)

# Save sf objects as rds ----

## County -----
write_rds(county_joined, "data/county_2010_19_sf.rds")
write_rds(zcta_joined, "data/zcta_2010_19_sf.rds")
write_rds(tract_joined, "data/tract_2010_19_sf.rds")
