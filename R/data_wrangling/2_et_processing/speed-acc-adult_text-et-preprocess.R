################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC EXPERIMENT
## read in data files and consolidate them into a .csv
##
## KM and AV 
################################################################################

## PRELIMINARIES
source("../../helper_functions/libraries_and_functions.R")

raw.data.path <- "../../data/1_raw_data/trio-adult/"
processed.data.path <- "../../data/2_cleaned_data/"

## LOOP TO READ IN FILES
all.data <- data.frame()
files <- dir(raw.data.path,pattern="*.txt")

for (file.name in files) {
  ## printe file name, so if loop breaks, we know where
  print(file.name)
  
  ## these are the two functions that are most meaningful
  d <- read.smi.idf(paste(raw.data.path,file.name,sep=""))
  d <- preprocess.data(d) 
  
  ## now here's where data get bound together
  all.data <- bind_rows(all.data, d)
}

## WRITE DATA OUT TO ZIPPED CSV FOR EASY ACCESS AND SMALL FILE SIZE
write_csv(all.data, path=paste0(processed.data.path, "speed_acc_processed_adult_data.csv.gz"))


