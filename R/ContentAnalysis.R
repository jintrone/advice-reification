evaluateAlpha<-function(code="fi") {
  j<-read.csv("../data/coding/liu.csv",stringsAsFactors = FALSE)
  c<-read.csv("../data/coding/thompson.csv",stringsAsFactors = FALSE)
  l<-read.csv("../data/coding/lengths.csv",stringsAsFactors = FALSE)
  j<-j[complete.cases(j),]
  c<-c[complete.cases(c),]
  j<-merge(j,l,by=c("corpus","uniqueId"))
  
  m<-merge(j,c,by=c("order"))
  
 
  if (code=="ut") {
    mat<-t(as.matrix(data.frame(x=as.integer(m[,paste(code,".x",sep="")]),y=as.integer(m[,paste(code,".y",sep="")]))))
    kripp.alpha(mat,"ordinal")
  } else {
    mat<-t(as.matrix(data.frame(x=as.integer(m[,paste(code,".x",sep="")])/as.integer(m$count),y=as.integer(m[,paste(code,".y",sep="")])/as.integer(m$count))))
    kripp.alpha(mat,"ratio")
  }
  #return(mat)
  
}

genStatisticalSample<-function() {
  j<-read.csv("../data/coding/liu.csv",stringsAsFactors = FALSE)
  c<-read.csv("../data/coding/thompson.csv",stringsAsFactors = FALSE)
  lngths<-read.csv("../data/coding/lengths.csv",stringsAsFactors = FALSE)
  #j[grepl(pattern="^\\s*$",j)]<-NA
  #return(j)
  #l$uid<-paste(l$corpus,l$uniqueId,sep="")
  lngths<-lngths %>% distinct(corpus,uniqueId,.keep_all=TRUE)
  m<-merge(j,c,by="order")
  print(nrow(m))
  m<- m %>% select(corpus.x,uniqueId.x,class.x,poster,uclass,fi.x,mi.x,os.x,us.x,ut.x,fi.y,mi.y,os.y,us.y,ut.y) 
  for (i in c("fi","mi","os","us","ut")) {
    l<-paste(i,".x",sep="")
    r<-paste(i,".y",sep="")
    m[,i]<-ifelse(is.na(m[,l]),ifelse(is.na(m[,r]),NA,as.integer(m[,r])),ifelse(is.na(m[,r]),as.integer(m[,l]),(as.integer(m[,l])+as.integer(m[,r]))/2))
  }
  #return(lngths)
  m<-merge(m,lngths,by.x=c("corpus.x","uniqueId.x"),by.y=c("corpus","uniqueId"))
  
  
  m<-m  %>% mutate(fi = fi/count, mi = mi/count, os=os/count,us=us/count) %>% select(class.x,corpus.x,uniqueId.x,fi,mi,os,us,ut,poster,uclass) %>% rename(class=class.x,corpus=corpus.x,uniqueId=uniqueId.x)
  return(m[complete.cases(m),])
  
  
}

