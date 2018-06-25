## Clear the decks
rm(list=ls())
#UNIVERSAL FUNCTIONS
require(reshape2)
require(ggplot2)
require(lubridate)
library(dplyr)
library(stringr)


if (Sys.info()["nodename"] == "BlackChopper.local")
{
  # Goggins Home Computer
  Sys.setenv("RProj_Data_Dir" = "/Volumes/SeansRAIDBaby/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data/")
} else {
  # Goggins Laptop
  Sys.setenv("RProj_Data_Dir" = "/Users/seanpgoggins/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data/")  
}

source("distpointline.r")
source("main-plotting-functions.R")
source("main-groovy-cutoffs.R")

data_dir = Sys.getenv("RProj_Data_Dir")

## Note: These 23 corpora are the ones that were in the paper going to Computers and Human Behavior. 
## Note: The original data directory had additional corpora node files generated.
corpora = c("add_and_adhd_exchange","alzheimers_exchange","asthma_exchange","back_pain_exchange","breast_cancer_exchange","cholesterol_management_exchange","diabetes_exchange","diet_exchange","digestive_disorders_exchange","epilepsy_exchange","fibromyalgia_exchange","fitness_and_exercise_exchange","hepatitis_exchange","hiv_and_aids_exchange","menopause_exchange","multiple_sclerosis_exchange","osteoporosis_exchange","pain_management_exchange","parenting_exchange","parkinsons_disease_exchange","relationships_and_coping_community","sex_and_relationships_exchange","sexual_conditions_and_stds_exchange")
test_corpora = c("add_and_adhd_exchange","alzheimers_exchange","asthma_exchange")
todo<-c("diabetes_exchange","diet_exchange","digestive_disorders_exchange","epilepsy_exchange","fibromyalgia_exchange","fitness_and_exercise_exchange","hepatitis_exchange","hiv_and_aids_exchange","menopause_exchange","multiple_sclerosis_exchange","osteoporosis_exchange","pain_management_exchange","parenting_exchange","parkinsons_disease_exchange","relationships_and_coping_community","sex_and_relationships_exchange","sexual_conditions_and_stds_exchange")
test_corpora = c("add_and_adhd_exchange","alzheimers_exchange","asthma_exchange")
ncorpora = c("allergies_exchange","anxiety_and_panic_disorders_exchange","bipolar_disorder_exchange","colorectal_cancer_exchange","depression_exchange","erectile_dysfunction_exchange","food_and_cooking_exchange","gynecology_exchange","heart_disease_exchange","hypertension_and_high_blood_pressure_exchange","infertility_and_reproduction_exchange","knee_and_hip_replacement_exchange","lupus_exchange","mens_health_community","migraines_and_headaches_exchange","newborn_and_baby_exchange","oral_health_exchange","osteoarthritis_exchange","pet_health_exchange","pregnancy_exchange","prostate_cancer_exchange","rheumatoid_arthritis_exchange","skin_and_beauty_exchange","skin_problems_and_treatments_exchange","sleep_disorders_exchange","smoking_cessation_exchange","sports_medicine_exchange","stroke_exchange","substance_abuse_exchange")

# enviroment variables
# dbname = "webmd_enriched"
# dbuser = "root"
# dbpassword = "lji123"

dbname = "webmd"
dbuser = "webmd"
dbpassword = "pickle"
host = "augurlabs.io"



# I/O Functions -----------------------------------------------------------

#Loads corpus specific data from the database. 
#Make sure to configure the relevant db parameters first
# corpus - the name of the corpus (see the list of corpora in the 'corpora' variable above)
loadPostingData<-function(corpus) {
  require(RMySQL)
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  rs<-dbSendQuery(con,paste("select qid, localID, date, poster from ",corpus,sep=""))
  data<-fetch(rs,n=-1)
  dbDisconnect(con)
  return(data)
}


# Loads tree data (the set of all CTCs in a corpus) specific to a corpus from a file
# Note that the tree files are produced with a specific "gap"
# which is parameter 'k' in the paper.  The 'buildTree' function in the groovy
# project is used to build the tree file
#  
#  corpus - the name of the corpus
#  start - the desired start date to be read in
#  end - the desired end date to be read in
#  topic - the topic method (part of the filename to be read in)
#  gap - the cutoff (k) that was used to generate the file (part of the filename)

## Tracing back to groovy and the cutoff k I find files with gaps that are 
## inconsistently named, which is consistent with the cutoffs.csv file
## Not finding anything with the gap = 28, actually 

loadData<- function(corpus,start="2009-01-01",end="2014-01-01",topic="NMF",gap=28) {
  file = str_c(data_dir,corpus,"_dag.",gap,".",topic,".csv",sep="")
  # print(file)
  tree<-read.csv(file)
  tree<-tree[tree$f.uniqueId!="NULL" & tree$t.uniqueId!="NULL",]
  tree$f.date<-as.POSIXct(tree$f.date,format="%Y-%m-%d %H:%M:%S")
  tree$t.date<-as.POSIXct(tree$t.date,format="%Y-%m-%d %H:%M:%S")
  return(tree)
}

#Loads all component files for all corpora from files on disk
# Component files describe each of the posts in a cross-threaded conversation
# which is effectively a connected graph in the graph of all posts for a given corpus
# See the 'pipelineToFile' function below
loadSummaryData<-function() {
  path<-stc_c(data_dir,"components/")
  bind_rows(lapply(list.files(path=path,pattern="*_nodedesc.csv"),function(x) read.csv(paste(path,x,sep=""))))
}


# Graph Construction ------------------------------------------------------
# Core graph function; generates graph from a tree file
# returns an igraph object
#   data - a tree, loaded by the 'loadData' command
buildBasicGraph<-function(data) {
  require(igraph)
  nodes<-data[,which(names(data) %in% c("f.uniqueId","f.poster","f.class","f.date","f.topic"))]
  names(nodes)<-c("nodeid","time","poster","user","topic")
  nodesb<-data[,which(names(data) %in% c("t.uniqueId","t.poster","t.class","t.date","t.topic"))]
  names(nodesb)<-c("nodeid","time","poster","user","topic")
  nodes<-rbind(nodes,nodesb)
  nodes$pid = as.numeric(as.factor(nodes$poster))
  nodes<-nodes %>% unique(.)
  edges<-data[as.character(data$f.poster)!=as.character(data$t.poster),which(names(data) %in% c("f.uniqueId","t.uniqueId","t.date","similarity"))]
  names(edges)<-c("from","to","time","similarity")
  g<-graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
}

# Helper function to load a graph, and then prune links that do not
# connect nodes that are similar enough (parameter 's' in the paper)
# also decorates each node with a component index (these become the CTCs)
#  data - the data (obtained via the 'loadData' command)
#  cutoff - the similarity cutoff (parameter 's')
#  graph - an optional graph to be passed in
buildGraph<-function(data=NULL,cutoff=0.0,graph=NULL) {
  if (is.null(graph)) {
    g<-buildBasicGraph(data)
   
  } else {
    
    g<-graph
  }
  if (cutoff>0) {
    g<-g-E(g)[which(E(g)$similarity<cutoff)]
  }
  set_vertex_attr(g,"comp",value=components(g,mode="weak")[['membership']])
}


# Analysis Functions ------------------------------------------------------

# Sensitivity analysis for parameter S (similarity).  Traces the number of components
# generated as we step through various cutoff values.  Generate a table with the number
# of components at each cutoff level
#  data - the tree data to be analyzed (from loadData())
#  step - the granularity of the step function
#  graph - optionally, the graph (if it was already generated - just for convenience; providing
#          the graph will cause the data parameter to be ignored)
analyzeComponents<-function(data=NULL,step = 0.1,graph=NULL) {
  if (is.null(graph)) {
    g<-buildBasicGraph(data)
  } else {
    g<-graph
  }
  result<-c()
  x<-seq(0,1,step)
  for (v in x) {
    g<-g-E(g)[which(E(g)$similarity<v)]
    result<-c(result,count_components(g,mode="weak"))
  }
  tibble(cutoff=x,comps=result)
} 


f<-function(x) {
  max(V(buildGraph(tree,x))$comp)
}

sensitivityAnalysis<-function(tree) {
  vf<-Vectorize(f)
  tibble(cutoff=0:20/40)%>%mutate(c=vf(cutoff))
}

### Seeking an empirical or literature based
### foundation for this elbow function in 
### response to reviewer feedback.

findElbow<-function(data) {
  data<-data %>% mutate(n=(comps-min(comps))/(max(comps)-min(comps))) %>% mutate(d=distancePointLine(cutoff,n,1,0))
  #return(data)
  data[which(data$d==max(data$d)),]$cutoff
}

describeData<-function(data=NULL,cutoff=NULL,graph=NULL) {
  if (is.null(graph)) {
    g<-buildGraph(data,cutoff)
  } else {
    g <- graph
  }
  num<-inspectComponents(g)
  c<-tibble(c0.5=(length(which(num$a<=5)==TRUE)),
                  c5.10=(length(which(num$a>5 & num$a<=10)==TRUE)),
                  c10.50=(length(which(num$a>10 & num$a<=50)==TRUE)),
                  c50.100=(length(which(num$a>50 & num$a<=100)==TRUE)),
                  c100=(length(which(num$a>100))))
  
  
  
  return(c)  
}


iwrap<-function(g,c,f) {
  sg<-getSubgraph(g,c)
  f(sg)
}

wrap<-Vectorize(iwrap,"c")

numUsers<-function(graph) {
  length(unique(V(graph)$poster))
  
}

numDays<-function(graph) {
  max(V(graph)$level)-min(V(graph)$level)
}

percentStarts<-function(g) {
  d <- degree(g,mode="in")
  length(d[which(d==0)])/length(d)
}

percentEnds<-function(g) {
  d <- degree(g,mode="out")
  length(d[which(d==0)])/length(d)
}

meanNonTerminalInDegree<-function(g) {
  d <- degree(g,mode="in")
  mean(d[which(d>0)])
}

meanNonTerminalOutDegree<-function(g) {
  d <- degree(g,mode="out")
  mean(d[which(d>0)])
}

numTriangesInGraph<-function(g) {
  sum(count_triangles(g))
}

meanTriangesInGraph<-function(g) {
  mean(count_triangles(g))
}

propNodesInTriangles<-function(g) {
  v<-count_triangles(g)
  length(v[which(v>0)])/length(v)
}

maxPath<-function(g,idx) {
  d<-distances(g,v=V(g)[idx],mode="in")
  d<-d[which(!is.infinite(d))]
  max(d)
}

inDegree<-function(g,idx) {
  d<-distances(g,v=V(g)[idx],mode="in")
  d<-d[which(!is.infinite(d))]
  max(d)
}

maxPath<-Vectorize(maxPath,vectorize.args = "idx")

applyToComponents<-function(graph,minsize=-1,maxsize=Inf) {
    c<-inspectComponents(graph)
    numcomps<-nrow(c)
    #%>% mutate_at(c(1),funs(f))
    
    a<-c[which(c$a>1),] %>% mutate_at(c(1),funs(u=wrap(f=numUsers,c=.,g=graph),
                                                                  d=wrap(f=numDays,c=.,g=graph),
                                                                  ps=wrap(f=percentStarts,c=.,g=graph),
                                                                  pe=wrap(f=percentEnds,c=.,g=graph),
                                                                  id=wrap(f=meanNonTerminalInDegree,c=.,g=graph),
                                                                  od=wrap(f=meanNonTerminalOutDegree,c=.,g=graph),
                                                                  nt=wrap(f=numTriangesInGraph,c=.,g=graph),
                                                                  mt=wrap(f=meanTriangesInGraph,c=.,g=graph),
                                                                  pt=wrap(f=propNodesInTriangles,c=.,g=graph))) %>%
        mutate(level=ifelse(a<5,1,ifelse(a<10,2,ifelse(a<50,3,ifelse(a<100,4,5)))))
     a.c<- a%>% group_by(level) %>% summarise(count=n(),pct=n()/numcomps)
     print(a.c)
     a<- a %>% group_by(level) %>% summarize_at(c(2:11),funs(median,IQR))
     a$count<-a.c$count
     a$pct<-a.c$pct
     return(a)
}



# Component Pipeline ------------------------------------------------------

# Give a graph with nodes labelled by component id list the components
# by size (the number of nodes) in decending order
#  graph - the graph to analyze
inspectComponents<-function(g) {
  tibble(c=V(g)$comp) %>% group_by(c) %>% summarise(a=n()) %>% arrange(-a)
}


## Main summarization function 
summarizeComponents<-function(graph,comp=NULL) {
  if (!is.null(comp)) {
    sg<-getSubgraph(graph,comp)
    print(comp)
    ##debugging notes
    # print("subgraph section executed")
    # print_all(sg)    
    png(paste0(data_dir, "components/", as.character((runif(1))), "test03.png"))
    plot.igraph(sg)
    dev.off()
  } else {
    print("original graph as subgraph")
    sg<-graph
  }

    df<-as.data.frame(lapply(list.vertex.attributes(sg),function(x) get.vertex.attribute(sg,x)),col.names = list.vertex.attributes(sg))
 
  vSearch <- as.data.frame(lapply(list.edge.attributes(sg),function(x) get.edge.attribute(sg,x)),col.names = list.edge.attributes(sg))
  
  df$otherVertex <- getParent(sg,V(sg))
  
  df$maxpth<-maxPath(sg,V(sg))
  df$indegree<-degree(sg,V(sg),mode="in")
  df$outdegree<-degree(sg,V(sg),mode="out")
  
  df$triangles<-count_triangles(sg)
  df$csize<-nrow(df)
  return(df)
}


getParent <- function(g, idx) {
  ## will generate the ego graph for the node
  ## 
  ## igraph
  ### this should give us an egograph 
  ## order = nodeID ... order = 1 is node and adjacent nodes
  ## order = 0 is the node itself
  ## we need to get the post ID off of the node that part of this .. 

  # nodesAround <- ego(graph, order=0, nodes = nodeID, mode = "in")
  d<-adjacent_vertices(g,v=V(g)[idx],mode="in")
  # d<-d[which(!is.infinite(d))]
    
  nodesAround <- d    
  print("D")
  print(d)
  # print(nodesAround$names)

  ## get the nodelist off the iGraph
  ## subtract out the nodID I passed in
  
  ## That will leave me with a list of 1 node
  
  ## that NodeID is, we think, the postID

  nodesAround
  # return("all of you")
  
}

vsummarizeComponents<-Vectorize(summarizeComponents,vectorize.args = "comp",SIMPLIFY=FALSE)


pipeline<-function(corpus,topic="NMF",gap = 28) {
  d<-loadData(corpus,topic=topic,gap = gap)
  elbow<-findElbow(analyzeComponents(d,step=.01))
  g <- buildGraph(d,elbow)
  
  d<-applyToComponents(graph = g,1,1000)
  d$cutoff<-elbow
  d$corpus<-corpus
  return(d)
}

# Runs the entire pipeline to generate component description file
# corpus - the name of the corpus
# topic - the topic method (DTM or NMF - DTM is not entirely supported)
# gaps - 
pipelineToFile<-function(corpus,topic="NMF",gaps=NULL) {
  if (!is.null(gaps)) {
    g<-gaps[gaps$corpus==corpus,]$gap
    # print(g)
    d<-loadData(corpus,topic=topic,gap=g)  
  } else {
    d<-loadData(corpus,topic=topic)
  }
  
  elbow<-findElbow(analyzeComponents(d,step=.01))
  g <- buildGraph(d,elbow)
  c<-inspectComponents(g)
  ## Resulting component is a tibble with
  ## two columns. 
  # c - component ID
  # a - number of nodes in the component 
  
  print("component class")
  print(class(c))
  print("component info")
  print(c)
  
  ## These get added to the printed dataframe at the end. 
  ## Its unclear to me what this is for ... level, path and triangels are set to 0
  
  
  # Removed after email discussion with Josh on June 25, 2018
  # others<- as.data.frame(lapply(list.vertex.attributes(g),function(x) get.vertex.attribute(g,x)),col.names = list.vertex.attributes(g))
  # others<-others[(others$comp %in% c[c$a==1,]$c),]
  # others$level<-0
  # others$maxpth<- 0
  # others$triangles<-0
  # others$csize<-1
  
  ## Debugging print statement
  print("binding rows")
  r<-bind_rows(vsummarizeComponents(g,c[c$a>1,]$c))
  # print(r)
  
  ## Others removed after discussion with Josh, via 
  ## email on June 25, 2018
  # r<-bind_rows(r,others) %>% mutate(corpus=corpus)
  
  
  # print(r)
  print(class(corpus))
  print(class(data_dir))
  print(paste(data_dir,"components/", corpus,"_nodedesc.csv",sep=""))
  write.csv(r,file=paste(data_dir,"components/", corpus,"_nodedesc.csv",sep=""))
  
#  write.csv(r,file=paste0(data_dir,"components/", corpus,"_nodedesc.csv", sep=""))
  print(paste("Done:",corpus))
  #return(r)
  
  
}

doAll<-function(corpora,gaps) {
#  rbind_all(lapply(corpora,function (x) pipeline(x,gap=gaps[gaps$corpus==x,]$gap )))
  bind_rows(lapply(corpora,function (x) pipeline(x,gap=gaps[gaps$corpus==x,]$gap )))
}

genSample<-function(corpora,gap=NULL) {
  lapply(corpora,function(x) pipelineToFile(x,gaps=gap))
}

cutmax<-function(v,breaks=100) {
  x<-as.character(cut(v,breaks))
  x<-substr(x,2,nchar(x)-1)
  as.vector(sapply(x,function(a) as.numeric(strsplit(a,",")[[1]][2])))
  
}










               