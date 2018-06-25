## Clear the decks
rm(list=ls())
#UNIVERSAL FUNCTIONS
require(reshape2)
require(ggplot2)
require(lubridate)
library(dplyr)
library(stringr)

# Goggins Laptop
# Sys.setenv("RProj_Data_Dir" = "/Users/seanpgoggins/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data")

# Goggins Home Computer
Sys.setenv("RProj_Data_Dir" = "/Volumes/SeansRAIDBaby/Dropbox/Work/00. Active Projects/305. Visualizing Reflexive Dynamics/Current VRD Projects/2017-Cscw/data")

source("distpointline.r")

data_dir = Sys.getenv("RProj_Data_Dir")

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


###############
# I/O FUNCTIONS
###############

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

#Loads interpost times (times between successive posts by a user) from a file
#Code to generate interpost times in Groovy project
loadInterpostData<-function() {
  read.csv(str_c(data_dir,"all.innerposttimes.csv"))
  
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
  print(file)
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

###################
#GRAPH CONSTRUCTION
###################


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


####################
# ANALYSIS FUNCTIONS
####################

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

# Give a graph with nodes labelled by component id list the components
# by size (the number of nodes) in decending order
#  graph - the graph to analyze
inspectComponents<-function(g) {
  tibble(c=V(g)$comp) %>% group_by(c) %>% summarise(a=n()) %>% arrange(-a)
}

# Generate the cutoff values based on interpost times
# Corresponds to parameter K
generateCutoffs<-function() {
  d<-loadInterpostData()
  d%>%group_by(corpus)%>%filter(delta>0)%>%summarise(M=mean(log(delta)),K=exp(mean(log(delta))+(2*sd(log(delta)))))
}





#######################
# PLOTTING FUNCTIONS
#######################

scaleTo<-function(x,y,min=-1,max=1) {
  ((x-min(x))/(max(y)-min(y)))*(max-min)-((max-min)/2)
}



cf<-function(x) {
  c("red","orange","blue")[match(x,c("C","P","XP"))]
}


# Get's the subgraph corresponding to a component
# Note that this function decorates nodes with a "level" property, corresponding to the day
# This is used in the sugiyama plots below
#  g - the graph to be analysed
#  comp - the component to be extracted
getSubgraph<-function(g,comp) {
  indices<-which(V(g)$comp==comp)
  sg<-induced_subgraph(g,indices)
  set_vertex_attr(sg,"level",value=floor((V(sg)$time - min(V(sg)$time))/(60*60*24)))
}

# Produce a simple edgelist for the graph
#   graph - the graph to be examined
inspectEdges<-function(graph) {
  tibble(from = head_of(graph,E(graph))$name,to = tail_of(graph,E(graph))$name)
}

# Plots a subcomponent (indicated by the id of the component) of a graph using the sugiyama layout
# The sugiyama layout is a layered layout, that places each node on a level - in this case the level is determined 
# by a discreted timestamp (NOTE: this function might be broken right now!)
#   graph - the graph to plot
#   comp - the component id (numeric) to plot
plotSubgraphAsSugiyama<-function(graph,comp) {
  sg<-getSubgraph(graph,comp) 
  #tmp<-tibble(v=V(sg),l=V(sg)$level)
  #tmp<-tmp %>% group_by(l) %>% mutate(c=as.vector(components((induced_subgraph(sg,v)),"weak")[['membership']]))
  #tmp<- tmp %>% mutate(label=(l*100)+c)
  #tmp<-arrange(tmp,label)
  
  #x<-data.frame(id=unique(tmp$label))
  #x$order<-1:nrow(x)
  #tmp<-merge(tmp,x,by.x="label",by.y="id")
  #sg<-contract(sg,tmp$order,vertex.attr.comb = list(name="concat","first"))
  #sg<-set_vertex_attr(sg,"size",value=as.vector(sapply(V(sg)$name,length)))
  
  #E(sg)$weight<-count_multiple(sg)
  #sg<-simplify(sg)
  #delete_edges(sg,which(head_of(sg,E(sg))$level==tail_of(sg,E(sg))$level))
  l<-layout_with_sugiyama(sg,layer=V(sg)$level,vgap=10,hgap=1)
  origvert <- c(rep(TRUE, vcount(sg)), rep(FALSE, nrow(l$layout.dummy)))
  realedge <- as_edgelist(l$extd_graph)[,2] <= vcount(sg)
  
  #print(scaleTo(l$extd_graph$layout[,2],))
  l$extd_graph$layout[,1]=scaleTo(l$extd_graph$layout[,1],l$extd_graph$layout[,1])
  l$extd_graph$layout[,2]=scaleTo(l$extd_graph$layout[,2],l$extd_graph$layout[,2],min=-4,max=4)
  #print(l$extd_graph$layout)
  
  plot(l$extd_graph,vertex.label.cex=.75,
       vertex.label.family="sans",
       vertex.label.dist=.5,
       edge.arrow.size=.25,
       rescale=FALSE,
       vertex.frame.color=NA,
       edge.width=.5,
       vertex.color = ifelse(origvert,V(sg)$pid, ""),
       #vertex.size=ifelse(origvert, V(sg)$size*5, 0),
       vertex.size=ifelse(origvert, 5, 0),
       vertex.shape=ifelse(origvert, "circle", "none"),
       vertex.label=ifelse(origvert, V(sg)$name,""),
       edge.arrow.mode=0,margin=c(0,0,10,10))
}


# Plots a set of indices of a graph using the sugiyama layout
# The sugiyama layout is a layered layout, that places each node on a level - in this case the level is determined 
# by a discreted timestamp (NOTE: this function might be broken right now!)
#   graph - the graph to plot
#   comp - the component id (numeric) to plot
plotSimpleSubgraph<-function(graph,indices) {
  sg<-getSubgraph(graph,indices) 
  l<-layout_with_sugiyama(sg,layers=V(sg)$level,vgap=10,hgap=1)
  print(l$layout)
  l$layout[,1]=scaleTo(l$layout[,1],l$layout[,1])
  l$layout[,2]=scaleTo(l$layout[,2],l$layout[,2],min=-2,max=2)

  plot(sg,layout=l$layout,vertex.label.cex=.75,
       edge.arrow.size=.25,
       rescale=FALSE,
       vertex.frame.color=NA,
       edge.width=.5,
       vertex.color = cf(V(sg)$user),
       #vertex.size=ifelse(origvert, V(sg)$size*5, 0),
       vertex.size=4,
       vertex.shape="circle",
       vertex.label=V(sg)$name,
       edge.arrow.mode=0,margin=c(0,0,10,10))
  
}

plotSubgraphAsTree<-function(graph,indices) {
  sg<-induced_subgraph(graph,indices)
  levels <- floor((V(sg)$time - min(V(sg)$time))/(60*60*24))
  r<-which(V(sg)$time == min(V(sg)$time))
  
  l<-layout_as_tree(sg,circular = TRUE, root=r) 
  
  plot(sg,
       edge.arrow.size=.25,
       rescale=TRUE,
       vertex.frame.color=NA,
       edge.width=.5,
       vertex.color = cf(V(sg)$user),
       vertex.size=5,
       vertex.shape="circle",
       vertex.label="",
       edge.arrow.mode=2, layout=layout_with_kk)
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


summarizeComponents<-function(graph,comp=NULL) {
  if (!is.null(comp)) {
    sg<-getSubgraph(graph,comp)
  } else {
    sg<-graph
  }
### All this is doing is printing out all the vertext attributes
  ### We need one edge attribute, which is the parent of each post
  ### 
  df<-as.data.frame(lapply(list.vertex.attributes(sg),function(x) get.vertex.attribute(sg,x)),col.names = list.vertex.attributes(sg))
  
  #  df2 <- as.data.frame(lapply(getParent(graph, )))
  ## We think the name of the vertex will be the nodeID that I pass into this 
  ## "Get Parent" function
  ## There's a column in the nodeDesc files which is the postID
  
  ## Need to know for each vertex, what the vertex on the other end of the edge is
  ## This "name" should be the "postid", like 158_9 or some such thing 
  
  ## apply a function to the dataframe (every row) to generate a new column 
  ### Name it "source"
  
  #TODO: KEY: Regenerate the components files so I can see what came before each row in the 
  ## THe component ... 
  ## Sometimes, will need to go back to the original thread to see the interaction
  ## Maybe need to do that sometimes but not all the times ... 
  
  
  df$maxpth<-maxPath(sg,V(sg))
  df$indegree<-degree(sg,V(sg),mode="in")
  df$outdegree<-degree(sg,V(sg),mode="out")
  
  df$triangles<-count_triangles(sg)
  df$csize<-nrow(df)
  return(df)
}

### Need a function to 
##

getParent <- function(graph, nodeID) {
  ## will generate the ego graph for the node
  ## 
  ## igraph
  ### this should give us an egograph 
  ## order = nodeID ... order = 1 is node and adjacent nodes
  ## order = 0 is the node itself
  ## we need to get the post ID off of the node that part of this .. 
  nodesAround <- ego(graph, order=1, nodes = nodeID, mode = "in")
  
  ## get the nodelist off the iGraph
  ## subtract out the nodID I passed in
  
  ## That will leave me with a list of 1 node
  
  ## that NodeID is, we think, the postID
  
  
  
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
    print(g)
    d<-loadData(corpus,topic=topic,gap=g)  
  } else {
    d<-loadData(corpus,topic=topic)
  }
  
  elbow<-findElbow(analyzeComponents(d,step=.01))
  g <- buildGraph(d,elbow)
  c<-inspectComponents(g)
  others<- as.data.frame(lapply(list.vertex.attributes(g),function(x) get.vertex.attribute(g,x)),col.names = list.vertex.attributes(g))
  others<-others[(others$comp %in% c[c$a==1,]$c),]
  others$level<-0
  others$maxpth<- 0
  others$triangles<-0
  others$csize<-1
  r<-bind_rows(vsummarizeComponents(g,c[c$a>1,]$c))
  
  r<-bind_rows(r,others) %>% mutate(corpus=corpus)
  
  write.csv(r,file=paste(corpus-spg,"_nodedesc.csv",sep=""))
  print(paste("Done:",corpus))
  #return(r)
  
  
}

doAll<-function(corpora,gaps) {
  rbind_all(lapply(corpora,function (x) pipeline(x,gap=gaps[gaps$corpus==x,]$gap )))
}

genSample<-function(corpora,gap=NULL) {
  lapply(corpora,function(x) pipelineToFile(x,gaps=gap))
}

cutmax<-function(v,breaks=100) {
  x<-as.character(cut(v,breaks))
  x<-substr(x,2,nchar(x)-1)
  as.vector(sapply(x,function(a) as.numeric(strsplit(a,",")[[1]][2])))
  
}


plotPathDescription<-function() {
  x<-read.csv("component_summary.csv")
  x4 <- x%>% filter(csize>1)%>%group_by(corpus,comp) %>% summarize(ind=mean(indegree[indegree>0]),outd=mean(outdegree[outdegree>0]),maxp=mean(maxpth[outdegree==0]),cs=max(csize))
  ggplot(x4,aes(x=ind,y=outd,color=maxp))+scale_color_viridis(name="Mean Path\nLength",direction = -1)+scale_x_log10()+scale_y_log10()+geom_jitter(size=.7)+theme_dark()+ylab("Mean Out Degree")+xlab("Mean In Degree")
 
}

# plotHeatmap<-function(data,cl,ul) {
#   ggplot(data,aes(csbin,urbin,))+geom_tile(aes(fill=rescale(clout),alpha=cs3),color="white")+
#     scale_fill_viridis(name="Coreness",direction=-1)+scale_alpha_identity()+
#     theme_minimal()+scale_x_continuous(breaks=c(1:10),labels=cl)+
#     scale_y_continuous(breaks=c(1:10),labels=ul)+theme(panel.grid.minor=element_blank(),panel.grid.major = element_blank())+
#     xlab("Conversation size")+ylab("User/post ratio")
# }








               