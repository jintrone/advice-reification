loadLiwcData <- function() {
  data<-read.csv("../data/components/LIWC.Results.Enriched.csv")
  data<-data[,-c(1,5)]
}

loadTestData<-function(corpus) {
  read.csv(paste("../data/",corpus,".tree.csv",sep=""))
}
