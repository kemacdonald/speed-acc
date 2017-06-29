# clear workspace
rm(list = ls())

# load libraries
library(reshape); library(plyr); library(grid)
library(lme4); library(knitr)
library(XML); library(gridExtra); 
library(magrittr); library(langcog); 
library(stringr); library(arm)
library(directlabels); library(lazyeval); 
library(forcats); library(RWiener)
library(GGally); library(lsr)
library(effsize); library(cowplot)
library(scales)

# load tidyvrse last, so no functions get masked
library(tidyverse)

# set ggplot theme
theme_set(
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

## Score trial function
## Categorizes trials as correct or incorrect in 2-AFC gaze task
## Takes in a row (trial), a start window, and an end window
## Returns whether that trial was a correct or incorrect shift

score_trial_type_lwl <- function(trial, start, end) { 
  start_col <- which(names(trial)==start)
  end_col <- which(names(trial)==end)
  trial_type <- "no_shift"
  rt <- NA
  response <- trial[which(names(trial)=="Response")]
  first_look_signer <- FALSE
  
  if (response == "D") {
    for (col in start_col:end_col) {
      prev_val <- trial[col-1]
      curr_val <- trial[col]
      next_val <- trial[col+1]
      
      if(is.na(curr_val)) { # break if we hit an NA
        break
        } 
      
      if(first_look_signer) {
        
        if(curr_val == "." & prev_val == "0.5") {rt <- names(trial[col])} # store RT
        
        if(curr_val == "." & next_val %in% c("0", "1", "0.5", ".5")) {
          if(next_val == "1") {
            trial_type <- "C_T"
          } else if(next_val == "0.5" | next_val == ".5") {
            trial_type <- "C_C"
          } else {
            trial_type <- "C_D"    
          }
          break
        }
      } else {
        ## check if current value is look to signer
        ## if it is then we should start checking for trial_type
        if(curr_val %in% c("0.5", ".5")) {
          first_look_signer <- TRUE
        }
      }
    }
  } else {
    trial_type <- "off_center"
  }
  return(c(trial_type, rt))
}

## Add trial numbers 
## Takes a participant's data frame and number of trials for each participant
## Returns the df with trial numbers added
add.tr.nums.fun <- function (df) {
  
  n_trials <- df %>% 
    select(subid, stimulus) %>% 
    unique() %>% 
    nrow()
  
  df %<>% select(subid, stimulus, t) %>% 
    arrange(t) %>% 
    select(subid, stimulus) %>% 
    unique() %>% 
    mutate(tr.num = 1:n_trials)
  
  return(df)
}

## Score trial for SMI eye tracking data
## Takes in a data frame for each trial and 
## Returns (1) an RT and (2) whether the shift was correct vs. incorrect

score_trial_et <- function(trial_df, crit_onset_type = "noun") {
  # filter trial to keep just that onset type
  trial_df %<>% filter(response_onset_type == crit_onset_type)
  
  print(paste(trial_df$tr.num[1], trial_df$subid[1]))
  
  # build variables to index each trial
  response.type.index <- paste0(crit_onset_type, ".onset") 
  t.filter.type <- paste0("t.rel.", crit_onset_type, " > 0")
  t.select.type <- paste0("t.rel.", crit_onset_type)
  
  # check if there is a shift in the trial after the critical onset
  # and record where the shift started and ended
  crit.window.responses <- trial_df %>% 
    filter_(t.filter.type) %>% 
    select_("target_looking", t.select.type) %>% 
    group_by(target_looking) %>% 
    summarise_(min_t = interp(~ min(x), x = as.name(t.select.type))) %>% 
    arrange(min_t)
  
  # store info about the shift
  shift.start <- crit.window.responses$target_looking[1]
  shift.info <-paste(crit.window.responses$target_looking[1], crit.window.responses$target_looking[2],
                     sep = "-")
  
  # check if there is only one "response" in the target_looking vector
  # if 1, then there was no shift (i.e., no change from response at crit.onset)
  if (nrow(crit.window.responses) == 1) {
    trial_score <- trial_df %>% 
      mutate(rt = NA, shift_type = "no_shift") %>% 
      select(rt, shift_type) 
  } else {
    # get the earliest time point when target looking switches from the critical onset value 
    trial_score <- trial_df %>% 
      filter_(t.filter.type) %>% 
      filter(target_looking != shift.start) %>% 
      select_(t.select.type, "target_looking") %>% 
      group_by(target_looking) %>% 
      summarise_(rt = interp(~ min(x), x = as.name(t.select.type))) %>% 
      filter(rt == min(rt)) %>% 
      mutate(shift_type = ifelse(shift.info == "center-target", "C_T", 
                                 ifelse(shift.info == "center-distracter", "C_D",
                                        ifelse(shift.info == "target-distracter", "T_D",
                                               ifelse(shift.info== "target-center", "T_C",
                                                      ifelse(shift.info == "distracter-target", "D_T",
                                                             ifelse(shift.info == "distracter-center", "D_C")))))),
             shift_accuracy = ifelse(shift_type == "C_T", "correct", "incorrect")) %>%
      select(rt, shift_type, shift_accuracy) 
  }
  
  # add the rt and score to the trial data frame
  trial_df <- cbind(trial_df, trial_score)
  
  return(trial_df)
}

## EWMA function
## Takes in a data frame
## Returns the EWMA statistic and upper threshold for each RT in the data frame

ewma_function <- function(df, rt_column = "RT", lambda = .01, cs = .5, sigma = .5, L = 1.5) {
  df <- arrange_(df, rt_column)
  results <- data.frame(rt = numeric(), cs = numeric(), ucl = numeric(), 
                        cond = character(), stringsAsFactors = F)
  
  for(row in 1:nrow(df)) {
    subj <- df[row, ]
    cond <- as.character(subj["condition"])
    acc <- as.integer(subj["correct"])
    rt <- as.numeric(subj["RT"])
    cs <- lambda*acc + (1-lambda)*cs # weighted average for each rt (row)
    ucl <- .5 + L*sigma*sqrt((lambda/(2 - lambda))*(1-((1-lambda)^(2*row)))) # threshold
    # add emwa params to results data frame
    subj$cs <- cs
    subj$ucl <- ucl
    results <- rbind(results, subj)
  }
  return(results)
}

## Compute guessing window for EWMA analysis
## Takes a data frame for each participant
## Returns a window of RTs where the participant was producing guesses 

compute_guessing_window <- function(sub_df) {
  # compute guessing window
  crit.window.responses <- sub_df %>%
    ungroup() %>% 
    dplyr::select(guess, RT) %>% 
    group_by(guess) %>% 
    summarise(min_rt = min(RT)) %>% 
    arrange(min_rt) 
  # if "resposne" not in the vector then ss was guessing on all shifts in that condition 
  if (!("response" %in% crit.window.responses$guess)) {
    # just use the diff between earliest and latest shifts
    sub_df$time_guessing <- max(sub_df$RT) - min(sub_df$RT) 
  } else {
    # here we use the diff between the earliest shift and the earliest "valid" (non-guess) shift
    sub_df$time_guessing <- max(crit.window.responses$min_rt) - min(crit.window.responses$min_rt)  
  }
  return(sub_df)
}

## Fit DDM function 
## Takes in a data frame that has been split by participant and condition
## Returns a data frame of DDM parameter values for that participant/condition
## Note that if the participant does not have any valid RTs, then it returns NAs for DDM vals

fit_ddm <- function(df, condition_col, subid_col, bysub = T, niter = 500) {
  # get the condition and subid values
  cond <- unique(df[[condition_col]])
  sub <- unique(df[[subid_col]])
  param_names <- c("separation", "non.decision", "bias", "drift")
  df %<>% 
    select(q, resp) %>% 
    as.data.frame()
  # fit ddm if there are valid responses to fit
  if (nrow(df) > 0) {
    # fit ddm for each participant 
    fit.vals <- optim(c(1, .1, .1, 1), wiener_deviance, control = list(maxit = niter),
                      dat=df, method="Nelder-Mead")
    pars <- cbind(data.frame(fit.vals = fit.vals$par), param = param_names, convergence = fit.vals$convergence) 
  } else {
    pars <- cbind(data.frame(fit.vals = c(NA, NA, NA, NA)), param = param_names, convergence = NA)
  }
  
  # add info to the sub_df and return
  if (bysub == T) {
    pars %<>% 
      mutate(subid = sub, condition = cond,
             fit.vals = round(fit.vals, 3))
  } else {
    pars %<>% 
      mutate(condition = cond, 
             fit.vals = round(fit.vals, 3))
  }
  
  return(pars)
}

## Remove outlier values
## takes in a data frame for an experimental condition, a stdev cutpoint, and a column name to compute over
## returns that data frame with extreme values +/- stdev cutpoint from the mean for that condition

remove_extreme_vals <- function(data_frame, sd_cut_val = 2, value_column) {
  m.val <- mean(data_frame[[value_column]])
  sd.val <- sd(data_frame[[value_column]])
  # compute cut points
  cut_point_upper <- round(m.val + (sd_cut_val * sd.val), 3)
  cut_point_lower <- round(m.val - (sd_cut_val * sd.val), 3)
  
  # standard eval stuff
  filter_criteria_upper <- interp(~y <= x, .values=list(y = as.name(value_column), x = cut_point_upper))
  filter_criteria_lower <- interp(~y >= x, .values=list(y = as.name(value_column), x = cut_point_lower))
  
  # filter and return df
  data_frame %<>% 
    mutate(cut_point_upper = cut_point_upper,
           cut_point_lower = cut_point_lower) %>% 
    filter_(filter_criteria_upper, 
            filter_criteria_lower)
  
  return(data_frame)
}