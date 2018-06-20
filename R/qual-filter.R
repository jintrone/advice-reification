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
require(ggpubr)

# Goggins Laptop
Sys.setenv("RProj_Data_Dir" = "/Users/seanpgoggins/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data")

# Goggins Home Computer
# Sys.setenv("RProj_Data_Dir" = "/Volumes/SeansRAIDBaby/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data")




source("distpointline.r")

data_dir = Sys.getenv("RProj_Data_Dir")

loadSummaryData<-function() {
  path <- str_c(data_dir,"/components/")
  bind_rows(lapply(list.files(path = path,pattern = "*_nodedesc.csv"),function(x) read.csv(paste(path,x,sep=""))))
}

## Description of data elements in the node description component summaries
#  level - From "main.R" : Note that this function decorates nodes with a "level" property, corresponding to the day. This is used in the sugiyama plots below
#  comp - the component to be extracted
#  post_id_native : mapped from "name" in component file to avoid keyword conflicts. More descriptive
#  post_timestamp : mapped from "time" in the component file to avoid keyword conflicts. More descriptive
#  pid: individual poster ID. I think this may be introduced by earlier code.  Not native.  Maps to a "poster"
#  poster: person making the post

#  csize - Number of posts in the component
#  triangles - 

loadAllPosts<-function(corpus) {
  require(RMySQL)
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  rs<-dbSendQuery(con,paste("select uniqueID, qid, localID, date, poster, inferred_replies, replyTo, cleancontent, title from ",corpus,sep=""))
  data<-fetch(rs,n=-1)
  dbDisconnect(con)
  return(data)
}

loadCTCPosts<-function(corpus) {
  require(RMySQL)
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  rs<-dbSendQuery(con,paste("select uniqueID, qid, localID, date, poster, inferred_replies, replyTo, cleancontent, title from ",corpus,sep=""))
  data<-fetch(rs,n=-1)
  dbDisconnect(con)
  return(data)
}

dbname = "webmd"
dbuser = "webmd"
dbpassword = "pickle"
host = "augurlabs.io"

qualPlotPathDescription<-function(CTCNow) {
  #x<-read.csv("component_summary.csv")
  x <- CTCNow
  x4 <- x %>% filter(csize>1) %>% group_by(corpus,comp,topic) %>% summarize(ind=mean(indegree[indegree>0]),outd=mean(outdegree[outdegree>0]),maxp=mean(maxpth[outdegree==0]),cs=max(csize)) 
  str(x4)
  ggplot(x4,res=600, height = 12, width = 12, aes(x=ind,y=outd,color=maxp))+scale_color_viridis(name="Mean Path\nLength",direction = -1)+scale_x_log10()+scale_y_log10()+geom_jitter(size=.7)+theme_dark()+ylab("Mean Out Degree")+xlab("Mean In Degree")+geom_label_repel(aes(label=paste(topic, " ", substr(corpus,1,9)), fill=factor(topic)), color='white', size=3.5)+
    theme(legend.position = "bottom")
  
}

CTCIndex <- loadSummaryData()

class(CTCIndex$time) = c('POSIXt','POSIXct')

# hist(CTCIndex$maxpth)

CTCIndex$comp <- as.factor(CTCIndex$comp)
CTCIndex$topic <- as.factor(CTCIndex$topic)

colnames(CTCIndex)[2] <- "uniqueID"
colnames(CTCIndex)[3] <- "post_timestamp"


## Removing NAs from indegree and out degree, as these seem to get introduced for every forum even a person is not in it
# How many rows have NA's
row.has.na <- apply(CTCIndex, 1, function(x){any(is.na(x))})
sum(row.has.na)

no.na <- apply(CTCIndex, 1, function(x){all(!is.na(x))})
sum(no.na)

nrow(CTCIndex)

CTCIndex.filtered <- CTCIndex[!row.has.na,]

nrow(CTCIndex)
nrow(CTCIndex.filtered)

# Somewhere along the line a row is introduced for every user in every forum. 
# This is indicated in the data with NA's in the in degree and outdegree, and the existance of forum information for users in every component.
# Removing any row with an NA cleans this up


### 

# qualPlotPathDescription()

CTCIndexADHD <- CTCIndex.filtered[(CTCIndex.filtered$comp==608) & CTCIndex.filtered$corpus=="anxiety_and_panic_disorders_exchange",]

CTCIADHDSorted <-CTCIndexADHD[order(CTCIndexADHD$post_timestamp, CTCIndexADHD$post_id_native),]

### Experimental

###

# CTCIndex_Ordered <- CTCIndex.filtered[order(CTCIndex.filtered$comp, CTCIndex.filtered$corpus, 
#                                             CTCIndex.filtered$maxpth, CTCIndex.filtered$csize, decreasing=TRUE),]

CTCIndex_Ordered <- CTCIndex.filtered[order(CTCIndex.filtered$csize, CTCIndex.filtered$comp, CTCIndex.filtered$corpus, 
                                            CTCIndex.filtered$maxpth, decreasing=TRUE),]

# Checking out the dataset distribution: 
min(CTCIndex_Ordered$csize)
max(CTCIndex_Ordered$csize)
median(CTCIndex_Ordered$csize)
mean(CTCIndex_Ordered$csize)
sd(CTCIndex_Ordered$csize)

# > min(CTCIndex_Ordered$csize)
# [1] 2
# > max(CTCIndex_Ordered$csize)
# [1] 862
# > median(CTCIndex_Ordered$csize)
# [1] 4
# > mean(CTCIndex_Ordered$csize)
# [1] 57.1414
# > sd(CTCIndex_Ordered$csize)
# [1] 170.2297
# > 

nrow(CTCIndex_Ordered[(CTCIndex_Ordered$csize > 8),])
nrow(CTCIndex_Ordered[(CTCIndex_Ordered$csize > 8) && (CTCIndex_Ordered$csize < 900),])

# plot(CTCIndex_Ordered$comp, log(CTCIndex_Ordered$csize))

csizedist <- CTCIndex_Ordered %>%
              group_by(csize) %>%
              summarise(counts=n())


ggplot(csizedist, aes(x = csize, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  geom_label_repel(aes(label=csize), color='black', size=3.5)+
  theme_pubclean()

component13 <- CTCIndex_Ordered[(CTCIndex_Ordered$comp==13),]


forumlist <- component13 %>%
              group_by(poster, corpus) %>%
              summarise(counts=n())

component13_posts <- loadAllPosts("sexual_conditions_and_stds_exchange")



## Next, load the posts in this component only

CTCPostIDs <- as.data.frame(component13$post_id_native)
CTC_Set <- merge(component13, component13_posts)

CTC_Set_DateOrder <- CTC_Set[order(CTC_Set$post_timestamp),]



