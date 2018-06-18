#CLEAR THE DECKS
rm(list=ls())
#UNIVERSAL FUNCTIONS
require(reshape2)
require(ggplot2)
require(ggrepel)
require(lubridate)
library(dplyr)
library(stringr)
require(igraph)
require(viridis)

source("distpointline.r")

data_dir = "../data/"

loadSummaryData<-function() {
  path <- str_c(data_dir,"/components/")
  bind_rows(lapply(list.files(path = path,pattern = "*_nodedesc.csv"),function(x) read.csv(paste(path,x,sep=""))))
}

CTCIndex <- loadSummaryData()

class(CTCIndex$time) = c('POSIXt','POSIXct')

# CTCLonger <- subset(CTCIndex, CTCIndex$maxpth > 3)

hist(CTCIndex$maxpth)

CTCIndex$comp <- as.factor(CTCIndex$comp)
CTCIndex$topic <- as.factor(CTCIndex$topic)

colnames(CTCIndex)[2] <- "post_id_native"
colnames(CTCIndex)[3] <- "post_timestamp"


# qualPlotPathDescription<-function() {
#   #x<-read.csv("component_summary.csv")
#   x <- CTCLonger
#   x4 <- x %>% filter(csize>1) %>% group_by(corpus,comp,topic) %>% summarize(ind=mean(indegree[indegree>0]),outd=mean(outdegree[outdegree>0]),maxp=mean(maxpth[outdegree==0]),cs=max(csize)) 
#   str(x4)
#   ggplot(x4,res=600, height = 12, width = 12, aes(x=ind,y=outd,color=maxp))+scale_color_viridis(name="Mean Path\nLength",direction = -1)+scale_x_log10()+scale_y_log10()+geom_jitter(size=.7)+theme_dark()+ylab("Mean Out Degree")+xlab("Mean In Degree")+geom_label_repel(aes(label=paste(comp), fill=factor(topic)), color='white', size=3.5)+
#     theme(legend.position = "bottom")
#   
# }

qualPlotPathDescription<-function(CTCNow) {
  #x<-read.csv("component_summary.csv")
  x <- CTCNow
  x4 <- x %>% filter(csize>1) %>% group_by(corpus,comp,topic) %>% summarize(ind=mean(indegree[indegree>0]),outd=mean(outdegree[outdegree>0]),maxp=mean(maxpth[outdegree==0]),cs=max(csize)) 
  str(x4)
  ggplot(x4,res=600, height = 12, width = 12, aes(x=ind,y=outd,color=maxp))+scale_color_viridis(name="Mean Path\nLength",direction = -1)+scale_x_log10()+scale_y_log10()+geom_jitter(size=.7)+theme_dark()+ylab("Mean Out Degree")+xlab("Mean In Degree")+geom_label_repel(aes(label=paste(topic, " ", substr(corpus,1,9)), fill=factor(topic)), color='white', size=3.5)+
    theme(legend.position = "bottom")
  
}

# qualPlotPathDescription()


# p + geom_label_repel(aes(label = rownames(df),
#                          fill = factor(cyl)), color = 'white',
#                      size = 3.5) +
#   theme(legend.position = "bottom")

#my.data.frame <- data[(data$V1 > 2) & (data$V2 < 4), ]


CTCIndexADHD <- CTCIndex[(CTCIndex$comp==608) & CTCIndex$corpus=="anxiety_and_panic_disorders_exchange",]

CTCIADHDSorted <-CTCIndexADHD[order(CTCIndexADHD$post_timestamp, CTCIndexADHD$post_id_native),]


newdata <- mtcars[order(mpg, cyl),]

