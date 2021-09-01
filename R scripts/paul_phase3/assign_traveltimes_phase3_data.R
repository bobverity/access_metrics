## Code to assign travel time to nearest Level II clinic to
## Phase-3 location data from Boyce Uganda Project (immunization)

## Libraries
library(tidyverse)
library(sf)
library(raster)

## Read cleaned version of data
dat <- read_csv("../../output/BoyceGatesVaccineAcc_DATA_2021-04-20_0744_clean_geo.csv")

## Convert to sf object
dat <- st_as_sf(dat %>% as.data.frame(),
                coords = c("LONG", "LAT"),
                crs = 4326)

## Subset to only id variable
dat <- dat[,1]

## Read in travel time, walking time
walk.time <- raster("../../data/travel_time/Cost_minutes_Level_II_III_Priv_walkonly.img")

# Quick Map
# tm_shape(walk.time) +
#   tm_raster(breaks = c(-Inf, seq(15, 120, 15), Inf)) +
# tm_shape(dat) +
#   tm_basemap("CartoDB.Voyager") +
#   tm_dots(size = 0.01)

## Extract values from walking time to data
dat$WALKTIME_LII <- extract(walk.time,
                            dat)


## Read in travel time, walking plus motorcycle time
walk.motorc.time <- raster("../../data/travel_time/Cost_minutes_Level_III_walkmotorcycle.img")

# Quick Map
# tm_shape(walk.motorc.time) +
#   tm_raster(breaks = c(-Inf, seq(15, 120, 15), Inf)) +
# tm_shape(dat) +
#   tm_basemap("CartoDB.Voyager") +
#   tm_dots(size = 0.01)

## Extract values from walking time to data
dat$WLKMOTTIME_LIII <- extract(walk.motorc.time,
                               dat)

## Write out just tabular information
write_csv(st_drop_geometry(dat),
          "../../output/BoyceGatesVaccineAcc_DATA_2021-04-20_0744_clean_geo_traveltimes.csv")
