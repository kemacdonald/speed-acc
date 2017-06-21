####### Randomize carrier phrases
library(tidyverse)

# make a vector with lenght 32 and equal numbers of 'hey' and 'look'
carriers <- replicate(16, 'hey') %>% 
  c(., replicate(16, 'look'))

# create random order by sampling from carriers vector

set.seed(7)

create_one_order <- function(carriers) {
  order <- sample(carriers, size = length(carriers), replace = F)
  order
}

d <- replicate(4, create_one_order(carriers)) %>% as.data.frame()

colnames(d) <- c('order1', 'order2', 'order3', 'order4')

write_csv(d, "../../data/trial_info/speed_acc_noise_carrier_orders.csv")

