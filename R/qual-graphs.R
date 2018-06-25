rm(list=ls())
#UNIVERSAL FUNCTIONS
require(reshape2)
require(ggplot2)
require(lubridate)
library(dplyr)
library(stringr)

source("main.R")

## Need to first load a list of cutoffs,
## which are fed into the algorithm using the 
## variable "gaps" for the whole set, then
## searching by corpora for the gap
gapper <- read.csv(file = paste0(data_dir,"/cutoffs.csv"))


                   