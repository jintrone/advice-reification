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
library(QCA)
library(GGally)
library(tnet)
library(geomnet)

# Goggins Laptop
# Sys.setenv("RProj_Data_Dir" = "/Users/seanpgoggins/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data")

# Goggins Home Computer
Sys.setenv("RProj_Data_Dir" = "/Volumes/SeansRAIDBaby/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data")

source("distpointline.r")

data_dir = Sys.getenv("RProj_Data_Dir")


buildComponentFile<-function() {
  path <- str_c(data_dir,"/components/")
  #bind_rows(lapply(list.files(path = path,pattern = "*_nodedesc.csv"),function(x) read.csv(paste(path,x,sep=""))), .id="id")
  #bind_rows(lapply(list.files(path = path,pattern = "*_nodedesc.csv"),function(x) as.data.frame(read.csv(paste(path,x,sep="")))), .id="id")
  datasets <- list.files(path = path,pattern = "*_nodedesc.csv")
    tryCatch(
    {for (counter in 1:length(datasets)){
      print(counter)
#      write.table(counter,"test.counter.csv", append=TRUE)
      this.file <- read.csv(paste(path,datasets[counter],sep=""))
      this.dataframe <- as.data.frame(this.file)
      ## Seems that data with NA for indegree and outdegree is sequential for
      ## all components and therefore not useful
      row.has.na <- apply(this.dataframe, 1, function(x){any(is.na(x))})
      this.dataframe <- this.dataframe[!row.has.na,]
      ##
      class(this.dataframe$time) = c('POSIXt','POSIXct')
      colnames(this.dataframe)[2] <- "uniqueID"
      colnames(this.dataframe)[3] <- "post_timestamp"
      this.dataframe$comp <- as.factor(this.dataframe$comp)
      this.dataframe$topic <- as.factor(this.dataframe$topic)
      print(datasets[counter])
      print(ncol(this.dataframe))
      if (counter == 1) {
        newdataframe <- this.dataframe
        write.table(this.dataframe, "all.csv", sep=",", col.names=TRUE, row.names = FALSE)
      } else 
      {
#        bind_rows(newdataframe, this.dataframe)
        write.table(this.dataframe, "all.csv", append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
        
      }
    }
    },
      warning = function(w)
      {
        print(w)
        print("warning")
        print(ncol(this.dataframe))
      },
      error = function(e)
      {
        print(e)
        print("error")
        print(ncol(this.dataframe))
      }
    )
}

loadAllPosts<-function(corpus) {
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

buildComponentFile()

test <- read.csv("all.csv")



