#### This script takes in raw .xml files that have trial level information for the speed_acc_child_gaze_experiment
#### and returns an order sheet with the target images, ROIs, and trial tag

######## Load libraries
source("../helper_functions/libraries_and_functions.R")

######## Define global variables
read.path <- "../../data/0b_trial_information/speed_acc_child_gaze_xml/"
stim.names <- c('ball', 'shoe', 'bottle', 'cookie', 'boat', 'juice', 'bunny', 'chicken')
center.fixations <- c("face")

########Read in .xml files (one for each trial)

files <- dir(read.path,pattern="*1920x1080.xml")

########Read in stimulus log .xml to get stimulus id tag and source name

log_files <- dir(read.path, pattern = "speed_acc*")
log_file_path <- "../../data/0b_trial_information/speed_acc_child_gaze_xml/"
stim.log <- list()

for(file in log_files) {
  this_path <- paste0(log_file_path,file)
  tmp_result <- xmlParse(file = this_path) %>% xmlToList()
  stim.log <- c(stim.log, tmp_result)
}

############# Loop through stimulus log list to get name and id tag for each trial
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
# note that we are collapsing a series of x/y coordinates into one numeric string
right_image_roi <- c("7685761024768", "144081019201080", "144081019201080")
left_image_roi <- c("0576256768", "08104801080")
center_image_rois <- c("3330691269", "3070717230", "3330688269", 
                       "62401296378", "57601344324", "62401289378",
                       "38401536648", "42201498562")

trial.level.df <- data.frame()

for (file in files) {
  
  print(file)
  
  trial <- xmlParse(paste(read.path, file, sep = "")) %>% 
    xmlToDataFrame(stringsAsFactors=F) %>% 
    select(Points, Name) 
  
  # center fixation was always a face in this experiment
  center.fix <- 'face'
  
  # get the name of the movie stim
  stimulus_name <- trial$Name[3]
  
  # use ROIs to create variable tracking left/right/center images
  # note that there is some complicated logic here to extract the different image types
  # if an image is not the right or the left, then it was in the movie file name, meaning it was the target
  trial %<>% 
    mutate(stim_location = ifelse(Points %in% left_image_roi, "left_image", 
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
           center = center.fix,
           stimulus_name = stimulus_name)
  
  trial.level.df %<>% bind_rows(., trial)
  
}

####### Join together the two relevant trial level data frames
trial_info_df %<>% left_join(., trial.level.df, by = "stimulus")

####### Read in timing information for each stimulus item and add to final trial info df
library(googlesheets)

trial_timing_gs <- gs_title("speed_acc_info_measurements")
trial_timing_grace <- trial_timing_gs %>% gs_read(ws = "grace")
trial_timing_olivia <- trial_timing_gs %>% gs_read(ws = "olivia")

trial.timing.df <- bind_rows(trial_timing_grace, trial_timing_olivia)

# joing with trial info
trial_info_df %<>% left_join(., trial.timing.df, by = "stimulus_name") 

####### Create variables to track gaze and noise information
trial_info_df %<>% 
  mutate(condition = gsub(condition, replacement = "nonoise", pattern = "no_noise")) %>% 
  separate(col = condition, sep = "_", into = c('gaze_condition', 'noise_condition')) %>% 
  mutate(noise_condition = gsub(noise_condition, pattern = "nonoise", replacement = "no_noise"),
         gaze_condition = ifelse(gaze_condition == "sa", "no_gaze", gaze_condition))  

####### Write to .csv
write_csv(trial_info_df, path = "../../data/trial_info/speed-acc-child-gaze-trial-info.csv")
