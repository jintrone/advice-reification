linkclassmap<- data.frame(code=c(0,1,2,3,4,5,6,7,8,9),userclass=c(NA,"c","c","c","p","p","p","xp","xp","xp"),direction=c(NA,"P","R","PR","P","R","PR","P","R","PR"))

decorateData<-function(b) {
  
  b<-b[,!(names(b) %in% c("numlinks","start","end"))]
  
  b$forum<-apply(b,1,FUN=function(x) strsplit(x,"_topic")[[1]][[1]])
  b$topic<-apply(b,1,FUN=function(x) paste("T",strsplit(x,"_topic")[[1]][[2]],sep=""))
  
  return(b)
}


getLinkClass<-function(f) {
  linkclassmap[linkclassmap$code==f,]  
}

#Generate fully disaggregated data
genanalysisFDA<-function(t) {
  s<-ddply(t,.(forum_topic, srcuser,targuser),summarize,numlinks=length(forum_topic),start=min(srctime),end=max(targtime),
           seg1=countif(seg,1),
           seg2=countif(seg,2),
           seg3=countif(seg,3),
           seg4=countif(seg,4),
           seg5=countif(seg,5),
           seg6=countif(seg,6),
           seg7=countif(seg,7),
           seg8=countif(seg,8),
           seg9=countif(seg,9),
           seg10=countif(seg,10))
  return(s)
}

genplotdata<-function(t) {
  b<-decorateData(genanalysis(t))
  b<-melt(b,id.vars = .(forum_topic))
  b$segment<-apply(b,1,FUN=function(x) as.integer(strsplit(x[2],"seg")[[1]][[2]]))
  b<-ddply(b,.(forum),mutate,scaled=value/max(value))
  return(b)
}

#summarize by number of links per segement in each forum topic
#NOTE:  Hackisshly eliminating the last 10 segs since they will never be populated with default date restrictions
genanalysis<-function(t) {
  ddply(t,.(forum_topic),summarize,numlinks=length(forum_topic),start=min(srctime),end=max(targtime),
        seg1=countif(seg,1),
        seg2=countif(seg,2),
        seg3=countif(seg,3),
        seg4=countif(seg,4),
        seg5=countif(seg,5),
        seg6=countif(seg,6),
        seg7=countif(seg,7),
        seg8=countif(seg,8),
        seg9=countif(seg,9),
        seg10=countif(seg,10))
}

showTrafficXSeg<-function(t) {
  b<-genplotdata(b)
  ggplot(data=b,aes(x=segment,y=scaled,color=forum_topic))+geom_line()+facet_wrap(~forum)+theme(legend.position="none")
}


#PARTIALLY AGGREGATED ANALYSIS

#summarize by number of links per segement in each forum topic by target class
genanalysisPA<-function(t) {
  s<-ddply(t,.(forum_topic, srcuser),summarize,numlinks=length(forum_topic),start=min(srctime),end=max(targtime),
           seg1=countif(seg,1),
           seg2=countif(seg,2),
           seg3=countif(seg,3),
           seg4=countif(seg,4),
           seg5=countif(seg,5),
           seg6=countif(seg,6),
           seg7=countif(seg,7),
           seg8=countif(seg,8),
           seg9=countif(seg,9),
           seg10=countif(seg,10))
  t<-ddply(t,.(forum_topic, targuser),summarize,numlinks=length(forum_topic),start=min(srctime),end=max(targtime),
           seg1=countif(seg,1),
           seg2=countif(seg,2),
           seg3=countif(seg,3),
           seg4=countif(seg,4),
           seg5=countif(seg,5),
           seg6=countif(seg,6),
           seg7=countif(seg,7),
           seg8=countif(seg,8),
           seg9=countif(seg,9),
           seg10=countif(seg,10))
  names(s)[match("srcuser",names(s))]<-"user"
  names(t)[match("targuser",names(t))]<-"user"
  t$utype="T"
  s$utype="S"
  rbind(s,t)
  
  
  #       seg11=countif(seg,11),
  #        seg12=countif(seg,12),
  #        seg13=countif(seg,13),
  #        seg14=countif(seg,14),
  ##        seg15=countif(seg,15),
  #        seg16=countif(seg,16),
  #        seg17=countif(seg,17),
  #        seg18=countif(seg,18),
  #        seg19=countif(seg,19),
  #        seg20=countif(seg,20))
}


splitseg<-function(x,div=2) {
  ceiling(as.integer(strsplit(x['variable'],"seg")[[1]][[2]])/div)
}

genplotdataPA<-function(t) {
  b<-decorateData(genanalysisPA(t))
  b<-melt(b,id.vars = .(forum,topic,forum_topic,user,utype))  
  b$segment<-apply(b,1,FUN=splitseg(x))
  b<-ddply(b,.(forum,utype),mutate,scaled=value/max(value))
  return(b)
}

showTrafficPA<-function(t,forums) {
  b<-genplotdataPA(t)
  b<-b[b$forum %in% forums,]
  ggplot(data=b,aes(x=segment,y=scaled,color=user))+geom_line()+facet_grid(utype~forum_topic)
  
}

#Generating compressed data for analysis
generateCompressedSet<-function(d,linkcutoff=20) {
  x<-genanalysis(d)
  scrub<-x[x$numlinks<=linkcutoff,]$forum_topic
  result <-genanalysisFDA(d)
  result<-result[!(result$forum_topic %in% scrub),]
  result<-decorateData(result)
  result$linktype = paste(result$srcuser,result$targuser,sep="_")
  result<-result[,!(names(result) %in% c("srcuser","targuser"))]
  result = melt(result,id.vars = .(forum,topic,forum_topic,linktype))
  result$segment<-apply(result,1,FUN=function(x) splitseg(x))
  result<-ddply(result,.(forum,topic,forum_topic,linktype,segment),summarise,value=sum(value))
  result<-ddply(result,.(forum_topic,segment),mutate,scaled_seg_sum=value/sum(value),scaled_seg_max=value/max(value))
  result<-ddply(result,.(forum_topic),mutate,scaled_topic_sum=value/sum(value),scaled_topic_max=value/max(value))
  result<-ddply(result,.(forum),mutate,scaled_forum_sum=value/sum(value),scaled_forum_max=value/max(value))
  return(result)
}

applyClusteringToCompressed<-function(data,pcacutoff=.80,method="scaled") {
  require(fpc)
  if (length(which(is.na(data[,method])))>0) {
    data[is.na(data[,method]),][,method]<-0
  }
  data$var<-paste(data$linktype,data$segment,sep="_")
  data<-data[,names(data) %in% c("forum_topic","var",method)]
  clustinput<-dcast(data,forum_topic~var,value.var=method,fill=0,fun.aggregate = sum)
  #clustinput.names<-clustinput[,1]
  #clustinput<-clustinput[,-1]
  #pca<-prcomp(clustinput,scale=TRUE)
  #return(pca)
  #clustindex = which(cumsum(pca$sdev**2/sum(pca$sdev**2))>=pcacutoff)[1]
  return(clustinput)
}

examineClustering<-function(d,label="Sum of squares") {
  # Determine number of clusters
  #d<-d[,-1]
  wss <- (nrow(d)-1)*sum(apply(d,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(d,centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab=label)
}

exhaustive<-function(data) {
  for (x in c("forum","seg","topic")) {
    for (y in c("max","sum")) {
      
      method=paste("scaled",x,y,sep="_")
      
      d<-generateCompressedSet(data)
      d<-applyClusteringToCompressed(d,method=method)
      examineClustering(d[,-1],label=method)
    }
  }
}


showTrafficCompressed<-function(t,forums=NA,method="scaled_forum_max",file=NA) {
  if (!is.na(forums)) {
    t<-t[t$forum %in% forums,]
  }
  names(t)[which(names(t)==method)]="plotval"
  # if (!is.na(file)) {
  #    pdf(file,width=10,height=10)
  #  }
  p<-ggplot(data=t,aes(x=segment,y=plotval,color=linktype))+geom_area(aes(color=linktype,fill=linktype),position="stack")+facet_wrap(~forum_topic)
  if (!is.na(file)) {
    ggsave(file,plot=p,width=15,height=15)
  }
}