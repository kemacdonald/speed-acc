##### Create timing measurements reliabilty document

## Clear the workspace
rm(list=ls())

## Load libraries that we need
library(tidyverse)
library(googlesheets)
library(magrittr)

### Read in the audio measurements google sheet
d.gs <- gs_title("speed_acc_info_measurements")
d.grace <- d.gs %>% gs_read(ws = 'grace')
d.olivia <- d.gs %>% gs_read(ws = 'olivia')

### Merge the different speakers into one data frame
d.all <- bind_rows(d.grace, d.olivia)

# set the seed so we get the same random sample each time
set.seed(234) 

### Extract stimuli items and key measurements
d.trimmed <- d.all %>% 
  filter(noise == "no_noise") %>% 
  sample_n(size = nrow(.) * 0.25) %>% 
  select(stimulus_name, speaker, noise) %>% 
  arrange(speaker, noise, stimulus_name)

### Create new columns for reliability coder to enter measurements
d.trimmed %<>% 
  mutate(noun_onset_sec = NA, 
         noun_onset_frames = NA,
         coder = "reliability")
  
### Write the reliability check data frame to a new google sheet
d.gs %>% 
  gs_ws_new(ws_title = "reliability_check", input = d.trimmed,
            trim = TRUE, verbose = FALSE, row_extent = nrow(d.trimmed), 
            col_extent = ncol(d.trimmed))