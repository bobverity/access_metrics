# kernel_permutation.R
#
# Author: Bob Verity
# Date: 2021-06-07
#
# Purpose:
# (this is an example header)
#
# ------------------------------------------------------------------

# load bobfunctions2 package. If not already installed this can be obtained from
# Github via the command devtools::install_github('bobverity/bobfunctions2')
library(bobfunctions2)

library(tidyverse)

# leave-one-out likelihood method for calculating optimal kernel density
# bandwidth from 2D spatial data
leave_one_out_bandwidth <- function(lon, lat, sigma_vec = seq(1, 10, l = 11)) {
  
  # get great circle distance matrix between observations
  d <- bobfunctions2::dist_gc(cbind(lon, lat))
  
  # apply likelihood for range of sigma (bandwidth) values
  loglike <- mapply(function(sigma) {
    
    # get normal density of each point from every other poine
    m <- dnorm(d, sd = sigma)
    diag(m) <- 0
    
    # average kernels and sum log-density over values
    sum(log(rowMeans(m)))
  }, sigma_vec)
  
  return(loglike)
}

# ------------------------------------------------------------------

# read in cleaned phase3 data
phase3 <- readRDS("output/phase3_processed.rds")

# subset to those with information on whether card was available
# NB. at least one vaccination event was recorded in all individuals carrying a card
phase3 <- phase3 %>%
  dplyr::filter(!is.na(card))

# subset to those with info on vaccine recall among those with no card
phase3 <- phase3 %>%
  dplyr::filter(!((card == 0) & (is.na(vacc_recall))))

# identify individuals who have never recieved any vaccine
phase3 <- phase3 %>%
  dplyr::mutate(no_vaccines = (card == 0) & (vacc_recall == 0))

# choose kernel density bandwidth
#sigma_vec <- seq(1, 5, l = 101)
#ll <- leave_one_out_bandwidth(lon = phase3$LONG,
#                        lat = phase3$LAT,
#                        sigma_vec = sigma_vec)
#plot(sigma_vec, ll)
#sigma_best <- sigma_vec[which.max(ll)]

sigma_best <- 0.3

# define grid spanning study region
n_long <- 201
n_lat <- 201
long_vec <- seq(30, 30.13, l = n_long)
lat_vec <- seq(0.23, 0.37, l = n_lat)
long_mat <- bobfunctions2::vec2mat(long_vec, lat_vec, 1)
lat_mat <- bobfunctions2::vec2mat(long_vec, lat_vec, 2)

# create array of kernels from every point in phase3 data
arr <- gc_arr <- array(dim = c(nrow(phase3), n_lat, n_long))
for (i in seq_len(nrow(phase3))) {
  if ((i %% 10) == 0) {
    message(sprintf("%s of %s", i, nrow(phase3)))
  }
  gc_dist <- bobfunctions2::lonlat_to_bearing(origin_lon = phase3$LONG[i],
                                              origin_lat = phase3$LAT[i],
                                              dest_lon = as.vector(long_mat),
                                              dest_lat = as.vector(lat_mat))$gc_dist
  
  gc_arr[i,,] <- matrix(gc_dist, nrow = n_lat)
  arr[i,,] <- matrix(dnorm(gc_dist, sd = sigma_best), nrow = n_lat)
}

# get risk surface of "control" (vaccinated) individuals
w <- which(!phase3$no_vaccines)
s_vacc <- colMeans(arr[w,,])

# quick plot
image(long_vec, lat_vec, t(s_vacc))
points(x = phase3$LONG, y = phase3$LAT,
       pch = 20, cex = 0.3, col = phase3$no_vaccines + 2)

# get risk surface of "case" (non-vaccinated) individuals
w <- which(phase3$no_vaccines)
s_novacc <- colMeans(arr[w,,])

# quick plot
image(long_vec, lat_vec, t(s_novacc))
points(x = phase3$LONG, y = phase3$LAT,
       pch = 20, cex = 0.3, col = phase3$no_vaccines + 2)

# get observed log relative risk surface
s_rel <- log(s_novacc / s_vacc)

# get null distribution of relative risk from permutation test
n_perms <- 1e3
perm_arr <- array(dim = c(n_perms, n_lat, n_long))
for (i in seq_len(n_perms)) {
  if ((i %% 10) == 0) {
    message(sprintf("%s of %s", i, n_perms))
  }
  w <- sample(nrow(phase3), sum(phase3$no_vaccines))
  perm_arr[i,,] <- log(colMeans(arr[w,,]) / s_vacc)
}
p_mat <- matrix(NA, n_lat, n_long)
for (i in seq_len(n_lat)) {
  for (j in seq_len(n_long)) {
    p_mat[i,j] <- mean(s_rel[i,j] > perm_arr[,i,j])
  }
}

# get point outside permutation distribution
mask_lower <- (p_mat < 0.025)
mask_upper <- (p_mat > 0.975)

# define plotting surface
s_plot <- s_rel

# mask out grid points distant from any observation
#min_mat <- matrix(NA, n_lat, n_long)
#for (i in seq_len(n_lat)) {
#  for (j in seq_len(n_long)) {
#    min_mat[i,j] <- min(gc_arr[,i,j])
#  }
#}
#s_plot[min_mat > 1] <- NA
#mask_upper[min_mat > 1] <- 0

# plot
viridis_palette <- "viridis"
plot1 <- ggplot() + theme_bw() +
  geom_raster(aes(x = long_mat, y = lat_mat, fill = s_plot)) +
  geom_point(aes(x = LONG, y = LAT, col = no_vaccines), size = 0.4, data = phase3) +
  geom_contour(aes(x = long_mat, y = lat_mat, z = as.numeric(mask_upper)), bins = 1,
               size = 0.5, linetype = "solid", colour = "red") +
  #geom_contour(aes(x = long_mat, y = lat_mat, z = as.numeric(mask_lower)), bins = 1,
  #             size = 0.25, linetype = "dotted", colour = "white") +
  scale_fill_viridis_c(option = viridis_palette, name = "log(relative risk\nunvaccinated)") +
  scale_color_manual(values = c(grey(0), "red"), name = "low vaccination") +
  coord_sf(expand = FALSE) +
  xlab("longitude") + ylab("latitude")

plot1

# save to file
saveRDS(list(plot = plot1,
             long_mat = long_mat,
             lat_mat = lat_mat,
             s_plot = s_plot,
             mask_upper = mask_upper),
        file = "output/kernel_density_clusters.rds")
