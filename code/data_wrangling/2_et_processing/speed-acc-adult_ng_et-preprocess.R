################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC EXPERIMENT
## read in data files and consolidate them into a .csv
##
################################################################################

## This script processes eye movement data for the adult experiment where we manipulaute
## Gaze and the signal-to-noise ratio of the language

## PRELIMINARIES
source(here::here("code/helper_functions/libraries_and_functions.R"))

raw.data.path <- here::here("data/1_raw_data/speed-acc-adult-ng/et_files/")
processed.data.path <- here::here("data/2_cleaned_data/")

## LOOP TO READ IN FILES
all.data <- data.frame()
files <- dir(raw.data.path,pattern="*.txt")

for (file.name in files) {
  ## print file name, so if loop breaks, we know where
  print(file.name)
  
  ## these are the two functions that are most meaningful
  d <- read.smi.idf(paste(raw.data.path,file.name,sep=""))
  d <- preprocess.data(d, x.max = 1920, y.max= 1080, samp.rate = 30) 
  
  ## now here's where data get bound together
  all.data <- bind_rows(all.data, d)
}

## WRITE DATA OUT TO ZIPPED CSV FOR EASY ACCESS AND SMALL FILE SIZE
write_csv(all.data, path=paste0(processed.data.path, "speed_acc_processed_adult_ng_data.csv.gz"))

## READ CALIBRATION INFORMATION AND SAVE TO DISK
calibration_path <- "data/1_raw_data/speed-acc-adult-ng/calibration_files"
calib_files <- list.files(here::here(calibration_path)) %>% here::here(calibration_path, .)
calib_col_names = c("order", "subid", "timestamp", "calibration_info")

d_calib <- calib_files %>% 
  map_df(read_calib_file, skip = 1, n_max = 1, 
         col_types = "cccc", col_names = calib_col_names)

write_csv(d_calib, here::here("data/3_final_merged_data/calibration/speed-acc-adult-ng-calibration.csv"))
