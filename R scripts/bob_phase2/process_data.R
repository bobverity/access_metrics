
# process_data.R
#
# Author: Bob Verity
# Date: 2021-03-08
#
# Purpose:
# Read in datasets, reformat and merge to produce convenient formats for
# downstream analyses.
#

library(dplyr)
library(sf)

# ----------------------------------------------------------------
# PROCESS VACCINATION DATA

# read in Ross data
main_dat <- read.csv("data/BoyceGatesVaccineAcc_DATA_2020-12-11_0924.csv")

# add sites
main_dat$ph1_site <- c("Bugoye HC III", "Ibanda HC II", "Katooke HC II", "Kibirizi HC II",
                       "Kisamba HC II", "Maghoma HC II", "Nyangonge HC II", "RMS Clinic")[main_dat$ph1_site + 1]

# add visit types
main_dat$visit_type <- c("Immunization", "OPD Visit", "Antenatal Clinic")[main_dat$visit_type + 1]

# add parish names
ph1_parish_names <- c("Bugoye", "Ibanda", "Katooke", "Kibirizi", "Muhambo")
main_dat$parish_name <- ph1_parish_names[match(main_dat$ph1_parish, 0:4)]

# add village names
village_name_list <- list(Bugoye = c("Bugoye", "Kanyanamigho", "Kisamba 1", "Kisamba 2", "Muramba 1", "Muramba 2", "Rwakingi 1A", "Rwakingi 1B"),
                          Ibanda = c("Ibanda 1", "Ibanda 2", "Nyakabugha", "Kiharara", "Mihunga", "Mirimbo", "Nyakalingijo", "Ruboni"),
                          Katooke = c("Katooke 1", "Katooke 2", "Kemihoko", "Kihindi", "Kinyangoye", "Kirongo", "Mapata", "Muleheya", "Nyangonge"),
                          Kibirizi = c("Bulindiguru", "Ihani", "Kasanzi", "Kibirizi", "Kikokera"),
                          Muhambo = c("Bunyangoni", "Katumba", "Maghoma", "Muhambo", "Nduguthu East", "Nduguthu West"))

main_dat$village_name <- mapply(function(i) {
  p <- main_dat$parish_name[i]
  vi <- main_dat[[sprintf("ph1_%s", tolower(p))]][i] + 1
  village_name_list[[p]][vi]
}, seq_len(nrow(main_dat)))


# ----------------------------------------------------------------
# PROCESS UBOS DATA

# read in parish shapefile
parish_dat <- st_read("data/KaseseVillagesUBOS20062122/Kasese_Villages_UBOS_2006.shp") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

# fix ambigiuous names
parish_dat$PNAME[parish_dat$PNAME == "BUHAMBO"] <- "MUHAMBO"

parish_dat$VNAME[parish_dat$VNAME == "MAWOMA"] <- "MAGHOMA"
parish_dat$VNAME[parish_dat$VNAME == "MURAMBA I"] <- "MURAMBA 1"
parish_dat$VNAME[parish_dat$VNAME == "MURAMBA II"] <- "MURAMBA 2"
parish_dat$VNAME[parish_dat$VNAME == "KISAMBA I"] <- "KISAMBA 1"
parish_dat$VNAME[parish_dat$VNAME == "KISAMBA II"] <- "KISAMBA 2"
parish_dat$VNAME[parish_dat$VNAME == "KATOOKE I"] <- "KATOOKE 1"
parish_dat$VNAME[parish_dat$VNAME == "KATOOKE II"] <- "KATOOKE 2"
parish_dat$VNAME[parish_dat$VNAME == "BUNYAKONI"] <- "BUNYANGONI"
parish_dat$VNAME[parish_dat$VNAME == "KANYIMINIGO"] <- "KANYANAMIGHO"
parish_dat$VNAME[parish_dat$VNAME == "KASAZI"] <- "KASANZI"
parish_dat$VNAME[parish_dat$VNAME == "IBANDA I"] <- "IBANDA 1"
parish_dat$VNAME[parish_dat$VNAME == "IBANDA II"] <- "IBANDA 2"
parish_dat$VNAME[parish_dat$VNAME == "MIRIBO"] <- "MIRIMBO"
parish_dat$VNAME[parish_dat$VNAME == "BOLINDIGURU"] <- "BULINDIGURU"
parish_dat$VNAME[parish_dat$VNAME == "NYAGONGE"] <- "NYANGONGE"
parish_dat$VNAME[parish_dat$VNAME == "NDUGUTU EAST"] <- "NDUGUTHU EAST"
parish_dat$VNAME[parish_dat$VNAME == "NDUGUTU WEST"] <- "NDUGUTHU WEST"
parish_dat$VNAME[parish_dat$VNAME == "KINYALIGOYE"] <- "KINYANGOYE"
parish_dat$VNAME[parish_dat$VNAME == "MUHUNGA"] <- "MIHUNGA"
parish_dat$VNAME[parish_dat$VNAME == "RWAKINGI I A"] <- "RWAKINGI 1A"
parish_dat$VNAME[parish_dat$VNAME == "RWAKINGI I B"] <- "RWAKINGI 1B"
parish_dat$VNAME[parish_dat$VNAME == "MULEHE"] <- "MULEHEYA"


# save processed data
saveRDS(parish_dat, file = "output/parish_dat_processed.rds")


# ----------------------------------------------------------------
# PROCESS DEMOG DATA

# read in census data
demog <- read.csv("data/Bugoye Sub-County Population Projections 2018-2020 (3).csv")

# fix ambigiuous names
demog$village[demog$village == "MURAMBA I"] <- "MURAMBA 1"
demog$village[demog$village == "MURAMBA II"] <- "MURAMBA 2"
demog$village[demog$village == "IBANDA I"] <- "IBANDA 1"
demog$village[demog$village == "IBANDA II"] <- "IBANDA 2"
demog$village[demog$village == "KISAMBA I"] <- "KISAMBA 1"
demog$village[demog$village == "KISAMBA II"] <- "KISAMBA 2"
demog$village[demog$village == "KATOOKE I"] <- "KATOOKE 1"
demog$village[demog$village == "KATOOKE II"] <- "KATOOKE 2"
demog$village[demog$village == "KANYAMINIGO"] <- "KANYANAMIGHO"
demog$village[demog$village == "NDUGUTU EAST"] <- "NDUGUTHU EAST"
demog$village[demog$village == "NDUGUTU WEST"] <- "NDUGUTHU WEST"
demog$village[demog$village == "MULEHE"] <- "MULEHEYA"
demog$village[demog$village == "RWAKINGI IB"] <- "RWAKINGI 1B"

# save to file
saveRDS(demog, "output/demog_processed.rds")

# ----------------------------------------------------------------
# MERGE VACCINE WITH UBOS DATA

# get intersection of each observation with UBOS data
parish_match <- mapply(function(i) {
  p <- st_sfc(st_point(c(main_dat$hh_long[i], main_dat$hh_lat[i])), crs = "+proj=longlat +datum=WGS84")
  ret <- unlist(suppressMessages(st_intersects(p, parish_dat)))
  if (length(ret) == 0) {
    ret <- NA
  }
  ret
}, seq_len(nrow(main_dat)))

# add UBOS matched locations to main data
main_dat$UBOS_DNAME <- parish_dat$DNAME[parish_match]
main_dat$UBOS_SNAME <- parish_dat$SNAME[parish_match]
main_dat$UBOS_PNAME <- parish_dat$PNAME[parish_match]
main_dat$UBOS_VNAME <- parish_dat$VNAME[parish_match]

# save to file
saveRDS(main_dat, file = "output/VaccineAcc_processed.rds")



