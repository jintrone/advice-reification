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
library(gtools)


getCorporaList<-function() {
  require(RMySQL)
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  rs<-dbSendQuery(con,"select distinct corpus from components")
  data<-fetch(rs,n=-1)
  dbDisconnect(con)
  return(data)
}

getComponentPosts <- function(corpus) {
  require(RMySQL)
  theCorpus <- corpus
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  theQuery <- paste("select b.comp, a.uniqueID, qid, localID, date, b.post_timestamp, a.poster, inferred_replies, replyTo, cleancontent, title, b.csize, b.maxpth, b.indegree, b.outdegree, b.triangles from add_and_adhd_exchange a, components b where a.uniqueID = b.uniqueID and b.corpus =", chr(34), theCorpus, chr(34), " order by b.csize desc, b.comp")
  print(theQuery)
  rs<-dbSendQuery(con, theQuery)
  data<-fetch(rs,n=-1)
  dbDisconnect(con)
  return(data)
}

buildQualLists <- function() {
  corporaList <- getCorporaList()
  for (corpora in 1:length(corporaList)) {
    theCorpora <- corporaList[corpora]
    thePosts <- getComponentPosts(theCorpora)
    str(thePosts)
  }
  
}

dbname = "webmd"
dbuser = "webmd"
dbpassword = "pickle"
host = "augurlabs.io"

test <- getCorporaList()



test2 <- getComponentPosts("add_and_adhd_exchange")

buildQualLists()

