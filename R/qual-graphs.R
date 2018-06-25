# Building the component stuff

rm(list=ls())
#UNIVERSAL FUNCTIONS
require(reshape2)
require(ggplot2)
require(lubridate)
library(dplyr)
library(stringr)
library(stringi)

source("main.R")

## Need to first load a list of cutoffs,
## which are fed into the algorithm using the 
## variable "gaps" for the whole set, then
## searching by corpora for the gap
gapper <- read.csv(file = paste0(data_dir,"cutoffs.csv"))
gapper <- transform(gapper, gap = round(K))

## Next, for each item in the cutoff that we care about, we
## Need to execute the core parts of Josh's "main.R"
for (i in 1:nrow(gapper))
{
  cutoff <- round(gapper[i,]$K)
  thisCorpus <- as.character(gapper[i,]$corpus)
 # print(corpus)
 # print(cutoff)
 # print("test")
  print(str(thisCorpus))
  print(class(thisCorpus))
  if (thisCorpus %in% corpora)
  {
   # print(paste(i," Yes ! "))
   # doAll(corpus,gapper)
    # print("breaker")
    # print(thisCorpus)
    # print("breaker")
    pipelineToFile(thisCorpus, topic = "NMF", gapper)
  }
}                   
