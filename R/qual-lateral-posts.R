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

source("main.R")

getCorporaList<-function() {
  require(RMySQL)
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  rs<-dbGetQuery(con,"select distinct corpus from components")
  data <- rs
#  dbDisconnect(con)
  return(data)
}

getComponentPosts <- function(corpus, con) {
  require(RMySQL)
#  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  theCorpus <- corpus
  # theQuery <- paste("select b.comp, a.uniqueID, qid, localID, date, b.post_timestamp, a.poster, inferred_replies, replyTo, cleancontent, title, b.csize, b.maxpth, b.indegree, b.outdegree, b.triangles from add_and_adhd_exchange a, components b where a.uniqueID = b.uniqueID and b.corpus =", chr(34), theCorpus, chr(34), " order by b.csize desc, b.comp")
  theQuery <- paste("SELECT components.uniqueID , components.post_timestamp , components.comp , components.neighbors , components.adjacent , components.componentVertexID , b.inferred_replies , b.title , b.content , b.poster , b.replyTo , components.`level` , components.poster , components.`user` , components.topic , components.pid , components.maxpth , components.indegree , components.outdegree , components.triangles , components.csize , components.corpus FROM components,", theCorpus, " b WHERE components.uniqueID = b.uniqueID AND components.csize > 2 AND components.corpus = \"", theCorpus, "\""," ORDER BY components.comp , components.post_timestamp",sep="")
 # print(theQuery)
  print(con)
  rs<-dbGetQuery(con, theQuery)
  # data<-fetch(rs,n=-1)
  data <- rs
  return(data)
}

buildQualLists <- function() {
  require(RMySQL)
  # con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  corporaList <- getCorporaList()
  print(corporaList)
  for (corpora in 1:nrow(corporaList)) {
    con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
    print(corpora)
    theCorpora <- corporaList[corpora,]
    print(theCorpora)
    if (corpora == 1){
      thePosts <- getComponentPosts(theCorpora, con)
      allPosts <- thePosts
     # print(thePosts)
    } else 
    {
      thePosts <- getComponentPosts(theCorpora, con)
      allPosts <- bind_rows(allPosts,thePosts)      
    }
    dbDisconnect(con)
  }
  return(allPosts)
  #  dbDisconnect(con)
}

dbname = "webmd"
dbuser = "webmd"
dbpassword = "pickle"
host = "augurlabs.io"

allPosts <- buildQualLists()

allPosts$corpus <- as.factor(allPosts$corpus)
allPosts$poster <- as.factor(allPosts$poster)
allPosts$replyTo <- as.factor(allPosts$replyTo)
allPosts$user <- as.factor(allPosts$user)
allPosts$topic <- as.factor(allPosts$topic)
allPosts$comp <- as.factor(allPosts$comp)
allPosts$uniqueID <- as.factor(allPosts$uniqueID)
allPosts$post_timestamp <- as.factor(allPosts$post_timestamp)

allPosts$content <- gsub("<APO>", "'",allPosts$content)

write.table(allPosts, file = paste(data_dir,"all_posts_for_components_larger_than_2.txt"), sep="|", qmethod = "double", col.names=TRUE, row.names = FALSE)

