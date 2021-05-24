## Code to read/clean Phase-3 location data from Boyce Uganda
## Project (immunization)

## Load libraries
library(tidyverse)
#library(sf)
#library(tmap)

## Read data
dat <- read_csv("../../data/BoyceGatesVaccineAcc_DATA_2021-04-20_0744.csv")


## Initial scan of position data shows a lot of issues...
##   Read in as character data
##   Let's use hh_lat and hh_long if available
##   Fill in missing from lat_tab and long_tab
##     But, note that lat_tab and long_tab have some reversed

## One entry has double ".."?
dat$lat_tab <- dat$lat_tab %>% str_replace("\\.\\.", "\\.") %>% as.numeric()

##
## Create single lat/long ----
##

## Make holder column for corrected *_tab versions
dat$lat_tab_upd <- dat$lat_tab
dat$long_tab_upd <- dat$long_tab

## Identify obvious errors
lat_long_flip <- which(dat$lat_tab > 1)
n_fix_flip <- length(lat_long_flip)

## Replace
dat$lat_tab[lat_long_flip] <- dat$long_tab_upd[lat_long_flip]
dat$long_tab[lat_long_flip] <- dat$lat_tab_upd[lat_long_flip]

## Create new field
dat$LAT <- dat$hh_lat
dat$LONG <- dat$hh_long

## Replace missing with "tablet" verions
dat$LAT[which(is.na(dat$LAT))] <- dat$lat_tab[which(is.na(dat$LAT))]
dat$LONG[which(is.na(dat$LONG))] <- dat$long_tab[which(is.na(dat$LONG))]

## Initial plot
plot(dat$LONG, dat$LAT)

## Looks like one is a transcription error 0.13xxxx instead of 0.31xxxx
##    but, we cannot be sure, unfortunately
## Looks like one is simply an error (missing LONG, both entries are LAT)
## Looks like one is transciption error begins LONG, ends with LAT
## So, we will remove them for now
dat <- dat %>% filter(LAT > 0.2 & LONG > 25 & LONG < 30.2)

## Initial plot
plot(dat$LONG, dat$LAT)

## Remove temp columns
dat <- dat %>% select(-c(long_tab_upd, lat_tab_upd))

## Write out data
write_csv(dat, "../../output/BoyceGatesVaccineAcc_DATA_2021-04-20_0744_clean_geo.csv")
