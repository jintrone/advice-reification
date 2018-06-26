# This gets sourced from main
require(reshape2)
require(ggplot2)
require(lubridate)
require(dplyr)
require(stringr)

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
  origvert <- c(rep(TRUE, vcount(sg)))
  
  realedge <- as_edgelist(l$extd_graph)[,2] <= vcount(sg)
  
  #print(scaleTo(l$extd_graph$layout[,2],))
  l$extd_graph$layout[,1]=scaleTo(l$extd_graph$layout[,1],l$extd_graph$layout[,1])
  l$extd_graph$layout[,2]=scaleTo(l$extd_graph$layout[,2],l$extd_graph$layout[,2],min=-4,max=4)
  #print(l$extd_graph$layout)
  
  graphTxt <- as.character(graph)
  png(paste(graphTxt, "-", comp, "_sugiyama.png"))
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
  dev.off()
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

plotSubgraphAsTree<-function(graph,indices,aCorpus) {
  sg<-induced_subgraph(graph,indices)
  levels <- floor((V(sg)$time - min(V(sg)$time))/(60*60*24))
  r<-which(V(sg)$time == min(V(sg)$time))
  
  l<-layout_as_tree(sg,circular = TRUE, root=r) 
  
  node <- as.character(V(sg)$name)
  comp <- V(sg)$comp
  vID <- V(sg)$componentVertexID
  corpus <- aCorpus
  nameGraph <- paste(corpus, "-", comp, "-", vID, "-", graph)
  png(paste0(data_dir,"components/", "graphs/", nameGraph,"-", node, "_subtree.png"))
  plot(sg,
       edge.arrow.size=.25,
       rescale=TRUE,
       vertex.frame.color=NA,
       edge.width=.5,
       vertex.color = cf(V(sg)$user),
       vertex.size=5,
       vertex.shape="circle",
       vertex.label= paste((V(sg)$poster),"-", (V(sg)$name)),
       edge.arrow.mode=2, layout=layout_with_kk)
  dev.off()
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
