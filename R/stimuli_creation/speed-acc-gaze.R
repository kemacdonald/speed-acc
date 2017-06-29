### Randomize gaze directions for the speed-acc experiment
### The goal is to create a vector of gaze directions randomly paired with the 8 target nouns

library(tidyverse)

# define some constants that we will use later
n_trials_block <- 32
n_gaze_trials_block <- n_trials_block / 2
n_blocks_adult <- 4
n_blocks_kid <- 2
target_nouns <- c('ball', 'shoe', 'bottle', 'cookie', 'boat', 'juice', 'bunny', 'chicken')

# make a vector with equal numbers of 'dl' and 'dr' paired with each target noun
gaze <- replicate(n_gaze_trials_block, 'dl') %>% 
  c(., replicate(n_gaze_trials_block, 'dr'))

# create random order by sampling from carriers vector

set.seed(7)

create_one_order <- function(carriers) {
  order <- sample(carriers, size = length(carriers), replace = F)
  order
}

d <- replicate(4, create_one_order(carriers)) %>% as.data.frame()

colnames(d) <- c('gaze1', 'gaze2', 'gaze3', 'gaze4')

write_csv(d, "../../data/trial_info/speed_acc_noise_carrier_orders.csv")
