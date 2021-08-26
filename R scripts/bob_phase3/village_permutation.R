# village_permutation.R
#
# Author: Bob Verity
# Date: 2021-06-07
#
# Purpose:
# Repeat of the kernel density permutation based method of identifying clusters
# of non-vaccinated people, bit this time at the village level rather than in
# continuous space. Note that in this simple case we can compute the null
# distribution exactly (hypergeometric distribution).
#
# ------------------------------------------------------------------

# load bobfunctions2 package. If not already installed this can be obtained from
# Github via the command devtools::install_github('bobverity/bobfunctions2')
library(bobfunctions2)

library(tidyverse)

# ------------------------------------------------------------------

# read in cleaned phase3 data
phase3 <- readRDS("output/phase3_processed.rds")

# subset to those with a village name
phase3 <- phase3 %>%
  dplyr::filter(!is.na(village_name))

# subset to those with information on whether card was available
# NB. at least one vaccination event was recorded in all individuals carrying a card
phase3 <- phase3 %>%
  dplyr::filter(!is.na(card))

# subset to those with info on vaccine recall among those with no card
phase3 <- phase3 %>%
  dplyr::filter(!((card == 0) & (is.na(vacc_recall))))

# identify individuals who have never recieved any vaccine
phase3 <- phase3 %>%
  dplyr::mutate(no_vaccines = (card == 0) & (vacc_recall <= 1))

# get proportion unvaccinated in each village
prop_obs <- phase3 %>%
  dplyr::group_by(village_name) %>%
  dplyr::summarise(numer = sum(no_vaccines), denom = n(), p = numer / denom)

# get lower and upper expected quantiles from hypergeometric distribution
prop_obs$upper <- prop_obs$lower <- NA
for (i in seq_len(nrow(prop_obs))) {
  q <- qhyper(p = c(0.025, 0.975),
              m = sum(phase3$no_vaccines),
              n = sum(!phase3$no_vaccines),
              k = prop_obs$denom[i]) / prop_obs$denom[i]
  prop_obs$lower[i] <- q[1]
  prop_obs$upper[i] <- q[2]
}

# plot
plot1 <- ggplot(prop_obs) + theme_bw() +
  geom_linerange(aes(x = village_name, ymin = lower, ymax = upper)) +
  geom_point(aes(x = village_name, y = p), col = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("village") + ylab("proportion no-vaccine")

plot1
  
# save to file
saveRDS(plot1,
        file = "output/village_permutation_clusters.rds")
