####### Randomize carrier phrases
library(tidyverse)

# make a vector with length 16 and equal numbers of 'hey' and 'look'


n_block2 <- 16
n_conditions <- 2

carriers <- replicate(n_block2 / n_conditions, 'hey') %>% 
  c(., replicate(n_block2 / n_conditions, 'look'))

# create random order by sampling from carriers vector

set.seed(7)

create_one_order <- function(carriers) {
  order <- sample(carriers, size = length(carriers), replace = F)
  order
}

n_orders <- 8

d <- replicate(n = n_orders, create_one_order(carriers)) %>% as.data.frame()

colnames(d) <- c('noise1', 'noise2', 'noise3', 'noise4', 'gaze1', 'gaze2', 'gaze3', 'gaze4')

## save carriers data frame to a file
write_csv(d, "../../data/trial_info/speed_acc_noise_carrier_orders.csv")


