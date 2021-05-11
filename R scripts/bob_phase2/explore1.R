
# explore1.R
#
# Author: Bob Verity
# Date: 2021-03-09
#
# Purpose:
# Initial exploration of phase1 data.
#
# NB. good leaflet maptype options include:
# 1, 10, 56, 110, 113, 116

# ------------------------------------------------------------------

library(sf)
library(tidyr)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(epitools)

# load bobfunctions2 package. If not already installed this can be obtained from
# Github via the command devtools::install_github('bobverity/bobfunctions2')
library(bobfunctions2)


# ----------------------------------------------------------------
# READ IN DATA

# read in Ross data
main_dat <- readRDS("output/VaccineAcc_processed.rds")

# drop problematic observations
main_dat <- main_dat %>%
  dplyr::filter(!is.na(hh_long) & !is.na(hh_lat) & !is.na(hh_long2) & !is.na(hh_lat2)) %>%
  dplyr::filter((hh_long == hh_long2) & (hh_lat == hh_lat2))

# read in parish shapefile
parish_dat <- readRDS("output/parish_dat_processed.rds") %>%
  dplyr::filter(PNAME %in% c("BUGOYE", "MUHAMBO", "IBANDA", "KATOOKE", "KIBIRIZI")) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")


# ----------------------------------------------------------------
# PROPORTION VISIT TYPE FOR EACH HEALTHCARE FACILITY

# produce plot
main_dat %>%
  dplyr::filter(!is.na(visit_type) & !is.na(ph1_site)) %>%
  dplyr::group_by(ph1_site, visit_type) %>%
  dplyr::count() %>%
  ggplot() + theme_bw() +
  geom_bar(aes(x = ph1_site, y = n, fill = visit_type), color = "black", position = "dodge", stat = "identity") +
  xlab("healthcare facility") + ylab("number of visits")


# ----------------------------------------------------------------
# DISTANCE TRAVELLED TO EACH HEATHCARE FACILITY

# read in healthcare site locations
df_site <- readRDS("data/healthcare_locations.rds")

# calculate great circle distance to visited healthcare site
main_dat$dist_visit <- mapply(function(i) {
  m <- match(main_dat$ph1_site[i], df_site$name)
  bobfunctions2::lonlat_to_bearing(main_dat$hh_long[i], main_dat$hh_lat[i],
                                   df_site$long[m], df_site$lat[m])$gc_dist
}, seq_len(nrow(main_dat)))

# calculate great circle distance to nearest healthcare site
main_dat$dist_nearest <- mapply(function(i) {
  min(bobfunctions2::lonlat_to_bearing(main_dat$hh_long[i], main_dat$hh_lat[i],
                                       df_site$long, df_site$lat)$gc_dist)
}, seq_len(nrow(main_dat)))


# plot average distance
main_dat %>%
  dplyr::filter(dist_visit < 20) %>%
  dplyr::filter(!is.na(visit_type)) %>%
  ggplot() + theme_bw() +
  geom_boxplot(aes(x = ph1_site, y = dist_visit, fill = visit_type))

# plot distance relative to closest
main_dat %>%
  dplyr::filter(dist_visit < 20) %>%
  dplyr::filter(!is.na(visit_type)) %>%
  ggplot() + theme_bw() +
  geom_boxplot(aes(x = ph1_site, y = dist_visit - dist_nearest, fill = visit_type))


# ----------------------------------------------------------------
# COMPARE VILLAGE CATCHMENTS

# define colour palette
pal <- colorFactor(
  palette = more_colours(40, raw_cols = brewer.pal(9, "Set1")),
  domain = toupper(main_dat$village_name)
)

# compare overlap
bobfunctions2::set_compare(toupper(main_dat$village_name),
                           main_dat$UBOS_VNAME)

# produce map
leaflet(main_dat) %>%
  addProviderTiles(leaflet::providers[[56]]) %>%
  addPolygons(data = parish_dat, color = ~pal(VNAME), weight = 1) %>%
  addCircles(lng = ~hh_long, lat = ~hh_lat, color = ~pal(toupper(village_name)), opacity = 1) %>%
  setView(lng = 30.07, lat = 0.30, zoom = 12)


# ----------------------------------------------------------------
# COMPARE TO CENSUS

# read in population projections
demog <- readRDS("output/demog_processed.rds") %>%
  dplyr::filter(year == 2020)

# compare visits to census
demog_compare <- main_dat %>%
  dplyr::group_by(village_name, visit_type) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::rename(village = village_name,
                visits = n) %>%
  dplyr::filter(!is.na(visit_type)) %>%
  dplyr::mutate(village = toupper(village)) %>%
  dplyr::left_join(demog) %>%
  dplyr::select(village, visit_type, visits, population) %>%
  dplyr::mutate(proportion = visits / population) %>%
  dplyr::filter(!is.na(population))


# make wide format table
demog_wide <- demog_compare %>%
  dplyr::select(village, visit_type, visits, population) %>%
  tidyr::pivot_wider(names_from = visit_type, values_from = visits) %>%
  dplyr::rename(Antenatal = 'Antenatal Clinic', OPD = "OPD Visit") %>%
  dplyr::group_by(village) %>%
  dplyr::summarise(population = population[1],
                   antenatal = mean(Antenatal, na.rm = TRUE),
                   antenatal_prop = antenatal / population,
                   Immunization = mean(Immunization, na.rm = TRUE),
                   Immunization_prop = Immunization / population,
                   OPD = mean(OPD, na.rm = TRUE),
                   OPD_prop = OPD / population)


# plot binomial confidence intervals
epitools::binom.exact(x = demog_compare$visits, n = round(demog_compare$population)) %>%
  dplyr::select(-c(proportion, x, n)) %>%
  dplyr::bind_cols(demog_compare) %>%
  ggplot() + theme_bw() +
  geom_pointrange(aes(x = village, y = proportion, ymin = lower, ymax = upper)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~visit_type, ncol = 1)

