## Code to read/clean Phase-3 location data from Boyce Uganda
## Project (immunization)

## Load libraries
library(tidyverse)
#library(sf)
#library(tmap)

## Read data
dat <- read_csv("../../output/BoyceGatesVaccineAcc_DATA_2021-04-20_0744_clean_geo.csv")



## Read in and create village field code data
par.codes <- read_csv("../../data/field_codes/ph1_parish_codes.csv")

## Bugoye
v.codes <- read_csv("../../data/field_codes/ph1_bugoye_codes.csv")
names(v.codes) <- c("V_code", "V_name")
v.codes$P_Name <- "Bugoye"
v.codes$P_code <- 0
village <- v.codes

## Ibanda
v.codes <- read_csv("../../data/field_codes/ph1_ibanda_codes.csv")
names(v.codes) <- c("V_code", "V_name")
v.codes$P_Name <- "Ibanda"
v.codes$P_code <- 1
village <- bind_rows(village, v.codes)

## Katooke
v.codes <- read_csv("../../data/field_codes/ph1_katooke_codes.csv")
names(v.codes) <- c("V_code", "V_name")
v.codes$P_Name <- "Katooke"
v.codes$P_code <- 2
village <- bind_rows(village, v.codes)

## Kibirizi
v.codes <- read_csv("../../data/field_codes/ph1_kibirizi_codes.csv")
names(v.codes) <- c("V_code", "V_name")
v.codes$P_Name <- "Kibirizi"
v.codes$P_code <- 3
village <- bind_rows(village, v.codes)

## Muhambo
v.codes <- read_csv("../../data/field_codes/ph1_muhambo_codes.csv")
names(v.codes) <- c("V_code", "V_name")
v.codes$P_Name <- "Muhambo"
v.codes$P_code <- 4
village <- bind_rows(village, v.codes)

## Create two digit village name
village$V_code2 <- paste0(village$P_code, village$V_code)


## Data cleaning step that i figured out after I ran
## the next command > some erroneous data, from
## what I can gather
which(dat$parish == 4 & !is.na(dat$v_bugoye))
which(dat$parish == 4 & !is.na(dat$v_kibirizi))
dat$v_bugoye[1008] <- NA
dat$v_kibirizi[1434] <- NA

## Add village information to imm data
dat$V_code2 <- apply(dat[,9:14],
                     1,
                     FUN = function(x) paste0(replace_na(x, ""), collapse = ""))

## Merge village name
dat_village_info <- merge(dat[,c(1,90)],
                          village[,c(5,2)],
                          by = "V_code2",
                          all.x = TRUE)
## Sort
dat_village_info <- dat_village_info[order(dat_village_info$ph3_redcap_id), c(2,1,3)]


## Write out table for reference and for joining to data
write_csv(village,
          "../../data/village_field_codes_combined.csv")
write_csv(dat_village_info,
          "../../output/BoyceGatesVaccineAcc_DATA_2021-04-20_0744_clean_village_join_info.csv")
