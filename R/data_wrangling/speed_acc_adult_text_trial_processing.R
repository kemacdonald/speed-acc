#### This script takes in raw .xml files that have trial level information 
#### and returns an order sheet with the target images, ROIs, and trial tag

## Load libraries
source("../helper_functions/libraries_and_functions.R")

## Define global variables
read.path <- "../../data/trial_info/trial_info_xml/"
stim.names <- c("shoe", "book", "cookie", "ball", "juice", "banana")
center.fixations <- c("text", "text-no-audio", "face")

## Read in .xml files (one for each trial)
files <- dir(read.path,pattern="*.xml")

## Read in stimulus log .xml to get stimulus id tag and source name
stim.log.order1 <- xmlParse(file = "../../data/trial_info/stim_names_order1.xml") %>% 
  xmlToList() 

stim.log.order2 <- xmlParse(file = "../../data/trial_info/stim_names_order2.xml") %>% 
  xmlToList()

stim.log <- c(stim.log.order1, stim.log.order2)

## Loop through stimulus log list to get name and id tag for each trial
trial_info_df <- data.frame()

for (index in 1:length(stim.log)) {
  # grab condition name
  condition <- stim.log[[index]]$Task
  if(is.null(condition)) {condition <- "NA"}
  # grab trial name
  stimulus <- stim.log[[index]]$TrialName
  # bind together in dataframe
  tmp.df <- data.frame(condition = condition, stimulus = stimulus, 
                       row.names = NULL, stringsAsFactors = F)
  trial_info_df %<>% bind_rows(., tmp.df)
}

# a little clean up
trial_info_df$stimulus <- gsub(trial_info_df$stimulus, pattern = ".avi", replacement = "") 
trial_info_df %<>% filter(condition != "NA") %>% unique()

####### loop through trial-level .xml files and extract relevant information using ROIs
# these ROI values were defined in the .xml stimulus properties files
right_image_roi <- c("7685761024768", "144081019201080", "144081019201080")
left_image_roi <- c("0576256768", "08104801080")
center_image_rois <- c("3330691269", "3070717230", "3330688269", "62401296378", "57601344324", "62401289378")

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
  trial %<>% mutate(stim_location = ifelse(Points %in% left_image_roi, "left_image", 
                                           ifelse(Points %in% right_image_roi, "right_image",
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
trial.timing.df <- read_csv("../../data/trial_info/speed-acc-adult-trial-timing-info.csv")
trial_info_df %<>% left_join(., trial.timing.df, by = "target_image") %>% 
  unique()

##### Add audio vs. no audio variable
trial_info_df %<>% mutate(audio = ifelse(str_detect(condition, pattern = "-no-audio") == T, 
                                         "no-audio", "audio"))

##### Write to .csv
write_csv(trial_info_df, path = "../../data/trial_info/speed-acc-adult1-trial-info-final.csv")
