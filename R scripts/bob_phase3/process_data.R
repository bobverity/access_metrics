# process_data.R
#
# Author: Bob Verity
# Date: 2021-06-07
#
# Purpose:
# Read in cleaned phase3 data from Paul's scripts. Further level of processing
# to add factor level names etc. Processed data saved to file.
#
# ------------------------------------------------------------------

library(tidyverse)

# read in processed data from Paul
main_dat <- read_csv("output/BoyceGatesVaccineAcc_DATA_2021-04-20_0744_clean_geo.csv")

# drop those without necessary information
main_dat <- main_dat %>%
  dplyr::filter(!is.na(parish))

# add parish names
parish_names <- c("Bugoye", "Ibanda", "Katooke", "Kibirizi", "Muhambo")
main_dat <- main_dat %>%
  dplyr::mutate(parish_name = parish_names[match(parish, 0:4)])

# add village names
village_name_list <- list(Bugoye = c("Bugoye", "Kanyanamigho", "Kisamba 1", "Kisamba 2", "Muramba 1", "Muramba 2", "Rwakingi 1A", "Rwakingi 1B"),
                          Ibanda = c("Ibanda 1", "Ibanda 2", "Nyakabugha", "Kiharara", "Mihunga", "Mirimbo", "Nyakalingijo", "Ruboni"),
                          Katooke = c("Katooke 1", "Katooke 2", "Kemihoko", "Kihindi", "Kinyangoye", "Kirongo", "Mapata", "Muleheya", "Nyangonge"),
                          Kibirizi = c("Bulindiguru", "Ihani", "Kasanzi", "Kibirizi", "Kikokera"),
                          Muhambo = c("Bunyangoni", "Katumba", "Maghoma", "Muhambo", "Nduguthu East", "Nduguthu West"))

main_dat$village_name <- unlist(mapply(function(i) {
  p <- main_dat$parish_name[i]
  vi <- main_dat[[sprintf("v_%s", tolower(p))]][i] + 1
  village_name_list[[p]][vi]
}, seq_len(nrow(main_dat))))

# save to file
saveRDS(main_dat, file = "output/phase3_processed.rds")
