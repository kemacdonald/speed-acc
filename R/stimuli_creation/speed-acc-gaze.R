### Randomize gaze directions for the speed-acc experiment
### The goal is to create a vector of gaze directions randomly paired with the 8 target nouns

library(tidyverse)

<<<<<<< HEAD
# define some constants that we will use later
n_trials_block <- 32
n_gaze_trials_block <- n_trials_block / 2
n_blocks_adult <- 4
n_blocks_kid <- 2
target_nouns <- c('ball', 'shoe', 'bottle', 'cookie', 'boat', 'juice', 'bunny', 'chicken')

# make a vector with equal numbers of 'dl' and 'dr' paired with each target noun
gaze <- replicate(n_gaze_trials_block, 'dl') %>% 
  c(., replicate(n_gaze_trials_block, 'dr'))
=======
# make a vector with lenght 32 and equal numbers of 'dl' and 'dr'
gaze <- replicate(n_trials_experiment / n_conditions, 'dl') %>% 
  c(., replicate(n_trials_experiment / n_conditions, 'dr'))
>>>>>>> 8cdccd83fa24e716d24744056c26639e6ae0d5f8

# create random order by sampling from gaze vector

set.seed(7)

create_one_order <- function(gaze) {
  order <- sample(gaze, size = length(gaze), replace = F)
  order
}

n_orders <- 4

d <- replicate(n = n_orders, create_one_order(gaze)) %>% as.data.frame()

colnames(d) <- c('gaze1', 'gaze2', 'gaze3', 'gaze4')

write_csv(d, "../../data/trial_info/speed_acc_gaze_direction_orders.csv")
