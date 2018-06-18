loadSATUsers<-function(corpus) {
  d<-read.csv(paste("../data/",corpus,".sir.csv",sep=""))
  d$date<-parse_date_time(d$date,orders="ymd HMS")
  return(d)
}

loadSIRUsers<-function(corpus,delta=14,filtered="none",cutoff="0.125") {
  d<-read.csv(paste("../data/",corpus,".users.sir.",filtered,".",cutoff,".",delta,".csv",sep=""))
  d$date<-parse_date_time(d$date,orders="ymd HMS")
  colnames(d)<-c("date","topic","class","state","value","percent")
  d[is.na(d$percent),]$percent<-0
  return(d)
}

loadSATPosts<-function(corpus) {
  d<-read.csv(paste("../data/",corpus,".posts.sat.csv",sep=""))
  
  d$date<-parse_date_time(d$date,orders="ymd HMS")
  
  return(d)
}

plotSIRValues<-function(d) {
  d<- d %>% group_by(class) %>% mutate(scaled = value/max(value))
  ggplot(d,aes(x=date,y=scaled,color=state))+geom_line()+facet_grid(class~topic)
}

