####### Randomize carrier phrases
library(tidyverse)

# make a vector with length 16 and equal numbers of 'hey' and 'look'
n_block <- 16
n_conditions <- 2

carriers_in_block <- replicate(n_block / n_conditions, 'hey') %>% 
  c(., replicate(n_block / n_conditions, 'look'))


# create random order by sampling from carriers vector

set.seed(7)

randomize_order <- function(block_carriers, n_blocks) {
  order <- vector()
  
  for (i in 1:n_blocks){
    tmp <- sample(carriers, size = length(carriers), replace = F)
    order <-  c(order, tmp)
  } 
  
  order
}

## create all the orders 

n_orders <- 4

d <- replicate(n = n_orders, randomize_order(carriers_in_block, n_blocks = 2)) %>% as.data.frame()

colnames(d) <- c('adult1', 'adult2', 'adult3', 'adult4')

## save carriers data frame to a file
write_csv(d, "../../data/trial_info/speed_acc_noise_carrier_orders_adult.csv")


