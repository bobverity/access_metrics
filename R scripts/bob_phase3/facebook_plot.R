# facebook_plot.R
#
# Author: Bob Verity
# Date: 2021-06-08
#
# Purpose:
# Compare facebook population density map with both phase II and phase III data.
#
# ------------------------------------------------------------------

# load bobfunctions2 package. If not already installed this can be obtained from
# Github via the command devtools::install_github('bobverity/bobfunctions2')
library(bobfunctions2)

library(raster)

# read in cleaned data
phase2 <- readRDS("output/VaccineAcc_processed.rds")
phase3 <- readRDS("output/phase3_processed.rds")

# read in healthcare site locations
df_site <- readRDS("data/healthcare_locations.rds")

# read in raster object
r <- readRDS("output/facebook_density_trim.rds")

# trim raster
b <- as(extent(30, 30.13, 0.23, 0.37), 'SpatialPolygons')
crs(b) <- crs(r)
r_trim <- crop(r, b)

plot(r_trim, asp = NA)

# convert to matrix
m <- as.matrix(r_trim)

# smooth using box blur
m[is.na(m)] <- 0
m_blur <- bobfunctions2::box_blur(m, d = 3)

# define lat and long vectors for this raster
long_vec <- seq(30, 30.13, l = ncol(m_blur))
lat_vec <- seq(0.23, 0.37, l = nrow(m_blur))
long_mat <- bobfunctions2::vec2mat(long_vec, rev(lat_vec), 1)
lat_mat <- bobfunctions2::vec2mat(long_vec, rev(lat_vec), 2)

# subset phase2 to immunisation visits only
phase2_sub <- phase2 %>%
  dplyr::filter(visit_type == "Immunization")

# plot phase2 immunisation only data on map
plot1 <- ggplot() + theme_bw() +
  geom_raster(aes(x = long_mat, y = lat_mat, fill = m_blur)) +
  geom_point(aes(x = hh_long, y = hh_lat), size = 0.5, data = phase2_sub) +
  geom_point(aes(x = long, y = lat), col = "red", size = 0.8, data = df_site) +
  coord_sf(xlim = range(long_vec), ylim = range(lat_vec), expand = FALSE) +
  scale_fill_viridis_c(name = "Facebook\npopulation\ndensity") +
  xlab("longitude") + ylab("latitude") + ggtitle("Phase II (immunization visits only)")

plot1

# plot phase3 data on map
plot2 <- ggplot() + theme_bw() +
  geom_raster(aes(x = long_mat, y = lat_mat, fill = m_blur)) +
  geom_point(aes(x = LONG, y = LAT), size = 0.5, data = phase3) +
  geom_point(aes(x = long, y = lat), col = "red", size = 0.8, data = df_site) +
  coord_sf(xlim = range(long_vec), ylim = range(lat_vec), expand = FALSE) +
  scale_fill_viridis_c(name = "Facebook\npopulation\ndensity") +
  xlab("longitude") + ylab("latitude") + ggtitle("Phase III")

plot2

# save to file
saveRDS(list(phase2 = plot1,
             phase3 = plot2,
             long_mat = long_mat,
             lat_mat = lat_mat,
             m_blur = m_blur),
        file = "output/facebook_phase_grob.rds")
