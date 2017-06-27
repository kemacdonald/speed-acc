####### Randomize gaze directions
library(tidyverse)

# make a vector with lenght 32 and equal numbers of 'dl' and 'dr'
gaze <- replicate(n_trials_experiment / n_conditions, 'dl') %>% 
  c(., replicate(n_trials_experiment / n_conditions, 'dr'))

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
