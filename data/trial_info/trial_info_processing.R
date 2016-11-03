#### This script takes in raw .xml files that have trial level information 
#### and returns an order sheet with the target images, ROIs, and trial tag


## Setup
library(XML); library(stringr); library(tidyverse); 
library(magrittr)
read.path <- "trial_info_xml/"
stim.names <- c("shoe", "book", "cookie", "ball", "juice", "banana")
center.fixations <- c("text", "text-no-audio", "face")

# these are the ROIs extracted from the .xml stimulus properties files
right_image_roi <- "7685761024768" 
left_image_roi <- "0576256768"
center_image_rois <- c("3330691269", "3070717230", "3330688269")

## Read in .xml files (one for each trial)
files <- dir(read.path,pattern="*.xml")

## Read in stimulus log .xml to get stimulus id tag and source name
stim.log <- xmlParse(file = "stimulus-log.xml") %>% 
  xmlToList()

##### Loop through stimulus log list to get name and id tag for each trial
stimulus.file.index <- 4
stimulus.name.index <- 2
trial_info_df <- data.frame()

for (trial in stim.log) {
  stim.log <- trial
}

for (index in 1:length(stim.log)) {
  source_name <- stim.log[[index]][stimulus.name.index]
  stimulus <- stim.log[[index]][stimulus.file.index]
  tmp.df <- data.frame(source_name = source_name, stimulus = stimulus, row.names = NULL, stringsAsFactors = F)
  trial_info_df %<>% bind_rows(., tmp.df)
}

trial_info_df$stimulus <- gsub(trial_info_df$stimulus, pattern = ".avi", replacement = "") 
trial_info_df %<>% filter(source_name != "Validation")

####### loop through trial-level .xml files and extract relevant information

trial.level.df <- data.frame()
for (file in files) {
  
  print(file)
  
  trial <- xmlParse(paste(read.path, file, sep = "")) %>% 
    xmlToDataFrame(stringsAsFactors=F) %>% 
    select(Points, Name) 
  
  # extract center fixation information from trial
  center.fix <- trial %>% 
    filter(Points %in% center_image_rois) %>% 
    select(Name) %>% 
    mutate(Name = ifelse(str_detect(Name, pattern = paste(center.fixations, collapse="|")) == T, 
                         str_extract(Name, pattern = paste(center.fixations, collapse="|")), "bullseye"))
  
  
  # use ROIs to create variable tracking left/right/center images
  # note that there is some complicated logic here to extract the different image types
  trial %<>% mutate(stim_location = ifelse(Points == left_image_roi, "left_image", 
                                           ifelse(Points == right_image_roi, "right_image",
                                                  "target_image"))) %>% 
    mutate(Name = str_extract(Name, pattern = paste(stim.names, collapse="|"))) %>% 
    select(-Points) 

  # get stimulus frome file name and strip .xml text
  trial.stimulus <- gsub(file, pattern = ".xml", replacement = "")
  
  # spread data to wide format since we only want one row per trial
  trial %<>%
    select(stim_location, Name) %>% 
    spread(key = stim_location, value = Name) %>% 
    mutate(stimulus = trial.stimulus,
           center = center.fix$Name)
  
  trial.level.df %<>% bind_rows(., trial)
  
}

###### Join together the two relevant trial level data frames
trial_info_df %<>% left_join(., trial.level.df, by = "stimulus")

###### Read in timing information for each stimlus item and add to final trial info df
trial.timing.df <- read_csv("speed-acc-trial-timing-info.csv")
trial_info_df %<>% left_join(., trial.timing.df, by = "target_image") %>% 
  unique()

##### Add audio vs. no audio variable
trial_info_df %<>% mutate(audio = ifelse(str_detect(source_name, pattern = "-no-audio") == T, 
                                         "no-audio", "audio"))

##### Write to .csv
write_csv(trial_info_df, path = "speed-acc-adult-trial-info.csv")
