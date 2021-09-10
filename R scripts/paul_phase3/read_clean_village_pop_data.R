## Code to read/clean village population data from Ross

## Load libraries
library(tidyverse)
library(stringr)
#library(sf)
#library(tmap)

## Read data
dat1 <- read_csv("../../data/Bugoye Sub-County Population Projections 2018-2020 (3).csv")
dat2 <- read_csv("../../data/updated Bugoye Sub-County Population Projections 2018-2020 (2).csv")


## Clean/prep data for merge
dat2$Sno <- NULL
dat2$population <- NA
dat2$households <- NA
names(dat2)[1:5] <- c("parish",
                      "village",
                      "category",
                      "year",
                      "under_5")

## Bind data
dat <- bind_rows(dat1,
                 dat2)

## Make table of unique villages in the pop data
dat_v <- tibble(V_name = unique(dat$village))

## Get village codes
v_codes <- read_csv("../../data/village_field_codes_combined.csv")

## Add title case versions
dat_v$V_name_t <- str_to_title(dat_v$V_name)

which(!(dat_v$V_name_t %in% v_codes$V_name))

## Replace I and II with 1 and 2
dat_v$V_name_t <- str_replace_all(dat_v$V_name_t,
                                  " Ii",
                                  " 2")
dat_v$V_name_t <- str_replace_all(dat_v$V_name_t,
                                  " I",
                                  " 1")

which(!(dat_v$V_name_t %in% v_codes$V_name))

## Start fixing
dat_v$V_name_t[2]
dat_v$V_name_t[2] <- "Kanyanamigho"
dat_v$V_name_t[4]
dat_v$V_name_t[4] <- "Rwakingi 1B"
dat_v$V_name_t[19]
dat_v$V_name_t[19] <- "Nduguthu East"
dat_v$V_name_t[20]
dat_v$V_name_t[20] <- "Nduguthu West"
dat_v$V_name_t[24]
dat_v$V_name_t[24] <- "Rwakingi 1A"
dat_v$V_name_t[32]
dat_v$V_name_t[32] <- "Muleheya"

which(!(dat_v$V_name_t %in% v_codes$V_name))


## Attach back to population data
dat <- merge(dat,
             dat_v,
             by.x = "village",
             by.y = "V_name",
             all.x = TRUE)

## Attach village code as well
dat <- merge(dat,
             v_codes,
             by.x = "V_name_t",
             by.y = "V_name",
             all.x = TRUE)

## Reorder, rename columns
dat <- dat[,c(1,9,10:12,2:8)]
names(dat)[1:5] <- c("v_name",
                     "v_code",
                     "parish_name",
                     "parish_code",
                     "v_code_unique")


## Write out table for reference and for joining to data
write_csv(dat,
          "../../output/Bugoye_subcounty_population_projections_2018_2021_tidy.csv")
