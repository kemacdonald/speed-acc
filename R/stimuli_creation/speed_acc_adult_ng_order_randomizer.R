####### Randomize orders for adult participants
library(tidyverse)

# make a vector with length 32 and equal numbers of '1,' '2,' '3,' and '4'
n_orders_in_exp <- 32
n_conditions <- 4

orders_in_experiment <- replicate(n_orders_in_exp / n_conditions, 'speed_acc_adult_ng_order1') %>% 
  c(., replicate(n_orders_in_exp / n_conditions, 'speed_acc_adult_ng_order2'))  %>%
  c(., replicate(n_orders_in_exp / n_conditions, 'speed_acc_adult_ng_order3')) %>%
  c(., replicate(n_orders_in_exp / n_conditions, 'speed_acc_adult_ng_order4'))

# test orders in experiment

n_orders_in_exp == length(orders_in_experiment)

# create random order by sampling 

set.seed(seed = 7)

## create all the orders 

n_orders <- 1

# shuffle vectors

order_results <- sample(orders_in_experiment) %>% as.data.frame()
colnames(order_results) <- 'order'

## save orders data frame to a file
write_csv(order_results, "../../data/trial_info/speed_acc_adult_ng_orders.csv")



