####### Randomize orders for child gaze experiment
library(tidyverse)

## define global variables
n_orders_in_exp <- 44 # this number was determined by an a priori power analysis 
n_orders <- 4

orders_in_experiment <- replicate(n_orders_in_exp / n_orders, 'order1') %>% 
  c(., replicate(n_orders_in_exp / n_orders, 'order2'))  %>%
  c(., replicate(n_orders_in_exp / n_orders, 'order3')) %>%
  c(., replicate(n_orders_in_exp / n_orders, 'order4'))

# test orders in experiment
n_orders_in_exp == length(orders_in_experiment)

# create random order by sampling 
set.seed(seed = 7)

## create all the orders 
#n_orders <- 1

# shuffle vectors

order_results <- sample(orders_in_experiment) %>% as.data.frame()
colnames(order_results) <- 'order'

## save carriers data frame to a file
write_csv(order_results, "../../data/trial_info/speed_acc_child_gaze_orders.csv")

### ADD more orders (8/17/17) because we had to run more than 44 kids to get the target sample
n_orders_added <- 20
orders_in_experiment_added <- replicate(n_orders_added / n_orders, 'order1') %>% 
  c(., replicate(n_orders_added / n_orders, 'order2'))  %>%
  c(., replicate(n_orders_added / n_orders, 'order3')) %>%
  c(., replicate(n_orders_added / n_orders, 'order4'))


# shuffle vectors
order_results_added <- sample(orders_in_experiment_added) %>% as.data.frame()
colnames(order_results_added) <- 'order'

write_csv(order_results_added, "../../../data/0b_trial_information/speed_acc_child_gaze_orders_added.csv")