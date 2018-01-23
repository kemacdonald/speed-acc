An information-seeking account of eye movements in signed and spoken language comprehension
===

This repository contains all of the data processing, analysis, and modelling code for a set of studies that test an information seeking account of eye movements in different language processing contexts (signed vs. spoken, written text vs. speech, clear speech vs. noisy speech, speech with eye gaze vs. speech without eye gaze).

The file naming conventions are:

* Signed vs. spoken language (children): `speed_acc_child_asl`
* Written text vs. spoken language (adults): `speed_acc_adult_text`
* Clear speech vs. noisy speech & Gaze vs. No-Gaze (adults): `speed_acc_adult_ng`
* Clear speech vs. noisy speech (children): `speed_acc_child_noise`
* Gaze vs. No-Gaze (children): `speed_acc_child_gaze`

You can find the raw data, data processing, and tidy data files here:

* [Data processing scripts](https://github.com/kemacdonald/speed-acc/tree/master/R/data_wrangling)
* [Raw data files](https://github.com/kemacdonald/speed-acc/tree/master/data/1_raw_data)
* [Tidy data files](https://github.com/kemacdonald/speed-acc/tree/master/data/3_final_merged_data) 

You can find the paper writeups here (note that you should be able to build these RMarkdown files from sratch):

* [RMarkdown manuscript for CogSci 2017](paper/cogsci2017/Speed-acc-cogsci.Rmd), which also contains all statistical models.
* [RMarkdown manuscript for CogSci 2018](paper/cogsci2018/Speed-acc-cogsci_2018.Rmd), which also contains all statistical models.

For more details about the analyses reported in the papers, see the following analysis files:

* [First shift Accuracy and RT viz](https://github.com/kemacdonald/speed-acc/tree/master/R/analysis/fst_shift_analyses)
* [DDM fit and viz](https://github.com/kemacdonald/speed-acc/tree/master/R/analysis/ddm)
* [EWMA fit and viz](https://github.com/kemacdonald/speed-acc/tree/master/R/analysis/ewma_guessing_analyses)
* [Bayesian Data Analysis](https://github.com/kemacdonald/speed-acc/tree/master/R/analysis/bda_models)