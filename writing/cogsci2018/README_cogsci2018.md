Speed-Acc CogSci 2018
-----------------

There are two key RMarkdown files in the paper repository for "Adults and preschoolers seek visual information to support language comprehension in noisy environments"

* The first file is: `speed-acc-cogsci2018-viz.Rmd`. It contains the R code for generating the behavioral results figure (Figure 2). The code pulls data and model summaries from the data directory and outputs a .png image file (e1_behav_results.png) to the "figs" directory. 

* The second file is: `Speed-acc-cogsci_2018.Rmd`. It contains the paper writeup along with the R code for generating plots of the model-based analyses.

All statisical models reported in the paper can be found here:

* [Speed-Acc CogSci 2018 BDA](https://github.com/kemacdonald/speed-acc/tree/master/R/analysis/bda_models)

Here is the session info when I build the paper. You can use this information to create the necessary R environment (i.e., install packages) for knitting the manuscript. 

**R version 3.4.1 (2017-06-30)**

**Platform:** x86_64-apple-darwin15.6.0 (64-bit) 

**locale:**
en_US.UTF-8||en_US.UTF-8||en_US.UTF-8||C||en_US.UTF-8||en_US.UTF-8

**attached base packages:** 

* grid 
* stats 
* graphics 
* grDevices 
* utils 
* datasets 
* methods 
* base 

**other attached packages:** 

* stringr(v.1.2.0) 
* dplyr(v.0.7.4) 
* purrr(v.0.2.4) 
* readr(v.1.1.1) 
* tidyr(v.0.7.2) 
* tibble(v.1.4.2) 
* tidyverse(v.1.2.1) 
* ggthemes(v.3.4.0) 
* png(v.0.1-7) 
* xtable(v.1.8-2) 
* rstanarm(v.2.17.2) 
* Rcpp(v.0.12.14) 
* cowplot(v.0.9.2) 
* ggplot2(v.2.2.1) 
* forcats(v.0.2.0) 
* magrittr(v.1.5) 
* pander(v.0.6.1) 
* knitr(v.1.18) 
* here(v.0.1) 