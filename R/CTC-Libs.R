## Shared R Methods

loadPostingData<-function(corpus) {
  require(RMySQL)
  con<-dbConnect(MySQL(),user=dbuser,password=dbpassword,dbname=dbname,host=host)
  rs<-dbSendQuery(con,paste("select qid, localID, date, poster from ",corpus,sep=""))
  data<-fetch(rs,n=-1)
  dbDisconnect(con)
  return(data)
}



