#### Cluster-based Permutation analysis helper functions
#### Note that these functions are specific to the Cesana-Arlotti 
#### variable names and will not generalize to other datasets

## Run T-test
# wrapper function that we can over a list of data frames
# takes a data frame
# does a t-test by trial type
run_t_test <- function(df, paired = FALSE, alternative = "two.sided") {
  t.test(ss_m ~ trial_type, 
         alternative = alternative,
         paired = paired,
         data = df)
}

## Run T-test By Time Bin
# wrapper around the run-test function
# df: data frame you want to do the t-test 
# alternative: type of t-test
# bin_cut: the upper threshold of the time bins that are common across conditions

t_test_by_bin <- function(df, alternative = "two.sided", paired = FALSE, bin_cut) {
  by_bin_df <- df %>% 
    group_by(Bin) %>% 
    nest()
  
  # Find and filter by the longest timepoint to make sure we have 
  # the same number of data points to comapre across conditions
  bin_cut_point <- ss_complete %>% 
    group_by(trial_type) %>% 
    summarise(max_bin = max(Bin)) %>% 
    pull(max_bin) %>% 
    min()
  
  by_bin_df %<>% filter(Bin <= bin_cut_point)
  by_bin_df %>% mutate(model = map(data, 
                                   run_t_test, 
                                   alternative = alternative,
                                   paired = paired))
}

## Extract T-stats
# takes a data frame with a list-column of t-tests 
# returns a data frame with the t-stats for each time bin

extract_t <- function(df, t_threshold, alternative = "two.sided") {
  if (alternative == "two.sided") {
    df %>% 
      mutate(glance = map(model, broom::glance)) %>% 
      unnest(glance, .drop = TRUE) %>% 
      select(Bin, statistic, p.value) %>% 
      filter(abs(statistic) >= t_threshold)  
  } else if (alternative == "lesser") {
    df %>% 
      mutate(glance = map(model, broom::glance)) %>% 
      unnest(glance, .drop = TRUE) %>% 
      select(Bin, statistic, p.value) %>% 
      filter(statistic <= t_threshold)
  } else if(alternative == "greater") {
    df %>% 
      mutate(glance = map(model, broom::glance)) %>% 
      unnest(glance, .drop = TRUE) %>% 
      select(Bin, statistic, p.value) %>% 
      filter(statistic >= t_threshold)
  } else {
    message('no alternative specified')
  }
  
}

## Sum the T-stats
# takes a data frame of t-stats produced by extract_t
# returns a either NA if there were not sig t-stats or 
# the largest summed t-stat for all the clusters 

sum_t_stats <- function(df) {
  if(nrow(df) == 0) {
    NA
  } else {
    largest_cluster <- df %>% 
      group_by(cluster) %>% 
      summarise(sum_t = sum(abs(statistic))) %>% 
      filter(sum_t == max(sum_t)) %>% 
      pull(cluster)
    
    df %>% 
      filter(cluster == largest_cluster) %>% 
      pull(statistic) %>% 
      sum()
  }
}


## Define Clusters
# make clusters for the bin vector (a little hacky but works. would be good to write some tests)
# function that takes a vector of bin numbers
# returns a vector of the same length with clusters defined
# works by checking whether the current bin number is not +1 greater than prev bin
# if it is greater than 1, then we have detected the end of a cluster and we should name it

define_clusters <- function(df) {
  if (nrow(df) == 0) {
    df
  } else {
    bin_vect <- df %>% pull(Bin)
    cluster_iter <- 1
    cluster_vector <- vector(length = length(bin_vect))
    # build the cluster vector
    for (bin in 1:length(bin_vect)) {
      if (bin == 1) {
        curr_cluster_name <- paste("cluster", as.character(cluster_iter), sep = "_")
        cluster_vector[bin] <- curr_cluster_name
      } else {
        if (bin_vect[bin] == bin_vect[bin-1] + 1 ) {
          cluster_vector[bin] <- curr_cluster_name
        } else {
          cluster_iter <- cluster_iter + 1
          curr_cluster_name <- paste("cluster", as.character(cluster_iter), sep = "_")
          cluster_vector[bin] <- curr_cluster_name
        } 
      }
    }
    mutate(df, cluster = cluster_vector)
    
  }
}

## Permute data
## Note this currently doesn't permute data 
## for within-subject designs

permute_data <- function(df, within = FALSE) {
  
  if (within) {
    subs_df_shuffle <-  df %>% 
      ungroup() %>% 
      distinct(Bin, Participant, trial_type, ss_m) %>% 
      group_by(Bin, Participant) %>% 
      mutate(old_trial_type = trial_type,
             trial_type = sample(trial_type, replace = F)) %>% 
      select(Bin, Participant, trial_type, ss_m)
    
    df %>% 
      ungroup() %>% 
      select(-trial_type, -condition, -ss_m) %>% 
      left_join(., subs_df_shuffle)
    
  } else {
    subs_df_shuffle <- df %>% 
      ungroup() %>% 
      distinct(Participant, trial_type) %>% 
      unique() %>%
      mutate(old_trial_type = trial_type,
             trial_type = sample(trial_type)) %>% 
      select(Participant, trial_type)
    
    df %>% 
      ungroup() %>% 
      select(-trial_type, -condition) %>% 
      left_join(., subs_df_shuffle, by = "Participant")
  }
  
}

