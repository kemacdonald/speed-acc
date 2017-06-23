####### Randomize gaze directions
library(tidyverse)

# make a vector with lenght 32 and equal numbers of 'dl' and 'dr'
carriers <- replicate(16, 'dl') %>% 
  c(., replicate(16, 'dr'))

# create random order by sampling from carriers vector

set.seed(7)

create_one_order <- function(carriers) {
  order <- sample(carriers, size = length(carriers), replace = F)
  order
}

d <- replicate(4, create_one_order(carriers)) %>% as.data.frame()

colnames(d) <- c('gaze1', 'gaze2', 'gaze3', 'gaze4')

write_csv(d, "../../data/trial_info/speed_acc_noise_carrier_orders.csv")
