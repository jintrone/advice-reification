## This gets sourced from Main
require(reshape2)
require(ggplot2)
require(lubridate)
require(dplyr)
require(stringr)

#Loads interpost times (times between successive posts by a user) from a file
#Code to generate interpost times in Groovy project
loadInterpostData<-function() {
  read.csv(str_c(data_dir,"all.innerposttimes.csv"))
  
}


# Generate the cutoff values based on interpost times
# Corresponds to parameter K
## This is only used to generate cutoffs for the groovy project
generateCutoffs<-function() {
  d<-loadInterpostData()
  d%>%group_by(corpus)%>%filter(delta>0)%>%summarise(M=mean(log(delta)),K=exp(mean(log(delta))+(2*sd(log(delta)))))
}





