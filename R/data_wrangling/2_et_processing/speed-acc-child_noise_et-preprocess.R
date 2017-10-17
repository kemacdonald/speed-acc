################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC EXPERIMENT
## read in data files and consolidate them into a .csv
##
################################################################################

## This script processes eye movement data for the adult experiment where we manipulaute
## Gaze and the signal-to-noise ratio of the language

## PRELIMINARIES
rm(list = ls())
library(readr); library(magrittr); library(pryr); library(stringr); 
source("../../helper_functions/libraries_and_functions.R")
library(tidyverse)

raw.data.path <- "../../../data/1_raw_data/speed-acc-child-noise/"
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

## WRITE DATA OUT TO ZIPPED CSV FOR EASY ACCESS AND SMALL FILE SIZE
write_csv(all.data, path=paste0(processed.data.path, "speed_acc_processed_child_noise.csv.gz"))
