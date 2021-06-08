# trim_facebook_density.R
#
# Author: Bob Verity
# Date: 2021-06-08
#
# Purpose:
# Read in Facebook population density estimate data from satellite imagery. Trim
# to study region and save trimmed raster to file.
#
# ------------------------------------------------------------------

library(raster)

# raw Uganda population density map downloaded from here (kept out of the git
# repos due to large filesize):
# https://data.humdata.org/dataset/highresolutionpopulationdensitymaps-uga

# load raster
str_name <- '/Users/rverity/Desktop/population_uga_2019-07-01.tif' 
r <- raster(str_name)

#plot(z)

# trim to study area
b <- as(extent(30, 30.17, 0.23, 0.4), 'SpatialPolygons')
crs(b) <- crs(r)
r_trim <- crop(r, b)

plot(r_trim)

# save to file as .rds
saveRDS(r_trim, file = "output/facebook_density_trim.rds")
