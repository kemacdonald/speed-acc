################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC EXPERIMENT
## read in data files and consolidate them into a .csv
##
################################################################################

## This script processes eye movement data for the adult experiment where we manipulaute
## Gaze and the signal-to-noise ratio of the language

## PRELIMINARIES
source("../../helper_functions/libraries_and_functions.R")

raw.data.path <- "../../../data/1_raw_data/speed-acc-adult-ng/"
processed.data.path <- "../../../data/2_cleaned_data/"

## LOOP TO READ IN FILES
all.data <- data.frame()
files <- dir(raw.data.path,pattern="*.txt")

for (file.name in files) {
  ## print file name, so if loop breaks, we know where
  print(file.name)
  
  ## these are the two functions that are most meaningful
  d <- read.smi.idf(paste(raw.data.path,file.name,sep=""))
  d <- preprocess.data(d, x.max = 1920, y.max= 1080, samp.rate = 30) 
  
  ## now here's where data get bound togetherq
  all.data <- bind_rows(all.data, d)
}

## some diagnostic plots
d_plot <- all.data %>%
  filter(!is.na(x), !is.na(y)) %>%
  mutate(x = ifelse(x < 1 | x > 1919, NA, x), 
         y = ifelse(y < 1 | y > 1079, NA, y))

stimulus_test_idx <- 10

d_plot %>% 
  filter(stimulus == stimulus[stimulus_test_idx]) %>% 
  ggplot(aes(x = t.stim, y = x)) + 
  geom_line(col = "darkgrey") + 
  geom_point(alpha = .3) + 
  ylim(c(0,1920)) + 
  geom_hline(yintercept = 1920/2, lty = 2) +
  facet_wrap(~subid) 

d_plot %>% 
  filter(stimulus == stimulus[stimulus_test_idx]) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_line(col = "darkgrey") + 
  geom_point(alpha = .3) + 
  xlim(c(0,1920)) + 
  ylim(c(0,1080)) +
  theme(legend.position = "top") +
  facet_wrap(~subid) 

# check how many unique trial labels we have for each participant
all.data %>% 
  mutate(subid = str_trim(subid)) %>% 
  group_by(subid) %>% 
  summarise(n = length(unique(stimulus))) %>% 
  kable()

## WRITE DATA OUT TO ZIPPED CSV FOR EASY ACCESS AND SMALL FILE SIZE
write_csv(all.data, path=paste0(processed.data.path, "speed_acc_processed_adult_ng_data.csv.gz"))