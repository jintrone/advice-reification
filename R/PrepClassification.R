library(tibble)
library(dplyr)
library(tidytext)
library(stringr)
library(data.table)

#library(RWeka)
inputFile <- "../data/classification/metafeatures.csv"
todo<-c("depression_exchange","diabetes_exchange","diet_exchange","digestive_disorders_exchange","epilepsy_exchange","erectile_dysfunction_exchange","fibromyalgia_exchange","fitness_and_exercise_exchange","food_and_cooking_exchange","gynecology_exchange","heart_disease_exchange","hepatitis_exchange","hiv_and_aids_exchange","hypertension_and_high_blood_pressure_exchange","infertility_and_reproduction_exchange","knee_and_hip_replacement_exchange","lupus_exchange","menopause_exchange","mens_health_community","migraines_and_headaches_exchange","multiple_sclerosis_exchange","newborn_and_baby_exchange","oral_health_exchange","osteoarthritis_exchange","osteoporosis_exchange","pain_management_exchange","parenting_exchange","parkinsons_disease_exchange","pet_health_exchange","pregnancy_exchange","prostate_cancer_exchange","relationships_and_coping_community","rheumatoid_arthritis_exchange","sex_and_relationships_exchange","sexual_conditions_and_stds_exchange","skin_and_beauty_exchange","skin_problems_and_treatments_exchange","sleep_disorders_exchange","smoking_cessation_exchange","sports_medicine_exchange","stroke_exchange","substance_abuse_exchange")

all_corpora <-c("add_and_adhd_exchange","allergies_exchange","alzheimers_exchange","anxiety_and_panic_disorders_exchange","asthma_exchange","back_pain_exchange","bipolar_disorder_exchange","breast_cancer_exchange","cholesterol_management_exchange","colorectal_cancer_exchange",todo)

createDtmFrame<-function() {

  
  forums <-
    read.csv(inputFile, header = TRUE, stringsAsFactors = FALSE)
  doc.v <-tibble(guid=forums$guid, text =forums$content)
  doc.v %>% unnest_tokens(word,text,token="regex",pattern="[^A-Za-z]+") %>% group_by(guid,word) %>% filter(str_length(word)>2) %>% summarise(count=n())
}

mergeMatrixAndDataframe<-function(df,mat) {
  d<-as.data.frame(as.matrix(mat))
  d$guid<-rownames(mat)
  df<-df%>%mutate(final_order=1:nrow(df))
  d<-left_join(df[,-which(names(df) %in% c("content","corpus","uid","ri","pi"))],d,by="guid") %>% arrange(final_order) %>% select(-final_order)
  d[is.na(d)]<-0
  return(d)
}

stepOne <- function() {
    d<-createDtmFrame() %>% cast_sparse(guid,word,count)
    forums <-
      read.csv(inputFile, header = TRUE, stringsAsFactors = FALSE)
    d<-mergeMatrixAndDataframe(forums,d)
    
    tmp<-data.frame(d[,-which(colnames(d)=="guid")])
    tmp$obj_fun<-forums$ri
    colnames(tmp)=c(1:ncol(tmp))
    write.csv(tmp,file="../data/classification/receivedinfo.csv")
    tmp[,ncol(tmp)]<-forums$pi
    write.csv(tmp,file="../data/classification/provideinfo.csv")

}

if (!exists("classification_frame")) {
  classification_frame<-createDtmFrame() %>% cast_sparse(guid,word,count)
}

if (!exists("classification_table")) {
  classification_table<-as.data.table(as.matrix(classification_frame))
}

generatePredictionFrame<-function(corpus) {
  d<-classification_frame
  print(paste("Number of target words ",ncol(d)))
  sample<- read.csv(paste("../data/classification/",corpus,".prediction.csv",sep=""),stringsAsFactors = FALSE)
  s.v<-tibble(guid=sample$guid, text =sample$content)
  s.v<- s.v %>% unnest_tokens(word,text,token="regex",pattern="[^A-Za-z]+") %>% group_by(guid,word) %>% 
    filter(str_length(word)>2) %>%  
    inner_join(tibble(word=colnames(d))) %>% 
    summarise(count=n()) %>% cast_sparse(guid,word,count) 
  
  print(paste("Number of corpus words ",ncol(s.v)))
  
  toadd<-colnames(d)[which(!(colnames(d) %in% colnames(s.v)))]
  s.v<-cbind(s.v,matrix(0,nrow=nrow(s.v),ncol=length(toadd),dimnames=list(NULL,toadd)))
  s.v<-s.v[,order(match(colnames(s.v),colnames(d)))]
  d<-mergeMatrixAndDataframe(sample,s.v)
  
  tmp<-data.frame(d[,-which(colnames(d)=="guid")])
  colnames(tmp)=c(1:ncol(tmp))
  write.csv(tmp,file=paste("../data/classification/",corpus,".topredict.csv",sep=""))
}

replaceNA<-function(dt) {
 
    # either of the following for loops
    # or by number (slightly faster than by name) :
    for (j in seq_len(ncol(dt)))
      set(dt,which(is.na(dt[[j]])),j,0)
 
}

generatePredictionFrame2<-function(corpus) {
  d<-classification_table
  print(paste("Number of target words ",ncol(d)))
 
  sample<-fread(paste("../data/classification/",corpus,".prediction.csv",sep=""),header = TRUE,sep=",")
  s.v<-sample[,.(guid,content)]
 
  s.v<- as.data.table(s.v %>% unnest_tokens(word,content,token="regex",pattern="[^A-Za-z]+"))
  s.v[,slen:=str_length(word)]
  s.v<-s.v[slen>2]
  
  dt<-data.table(word=colnames(d))
  s.v<-s.v[dt,on="word",nomatch=0]
 
  s.v<-s.v[,.(count=.N),by=.(guid,word)]
  #print(is.data.table(s.v))
  s.v<-dcast.data.table(s.v,guid~word,value.var="count")
  toadd<-setdiff(dt$word,colnames(s.v))
  #return(toadd)
  s.v[,(toadd):=0]
  #return(s.v)
  #s.v[,(toadd):=0]
  setcolorder(s.v,c("guid",dt$word))
  s.v<-merge(sample[,.(guid,qs,depth,tp,prank)],s.v,by="guid",all.x=TRUE)
  replaceNA(s.v)
  #return(s.v)
  colnames(s.v)<-c("",1:(ncol(s.v)-1))
  fwrite(s.v,file=paste("../data/classification/",corpus,".topredict.csv",sep=""),nThread=4)
  #%>% group_by(guid,content) %>% 
  #  filter(str_length(content)>2)
  
  #  inner_join(tibble(word=colnames(d))) %>% 
  #  summarise(count=n()) %>% cast_sparse(guid,word,count) 
  
  #print(paste("Number of corpus words ",ncol(s.v)))
  
  #toadd<-colnames(d)[which(!(colnames(d) %in% colnames(s.v)))]
  #s.v<-cbind(s.v,matrix(0,nrow=nrow(s.v),ncol=length(toadd),dimnames=list(NULL,toadd)))
  #s.v<-s.v[,order(match(colnames(s.v),colnames(d)))]
  #d<-mergeMatrixAndDataframe(sample,s.v)
  
  #tmp<-data.frame(d[,-which(colnames(d)=="guid")])
  #colnames(tmp)=c(1:ncol(tmp))
  #write.csv(tmp,file=paste("../data/classification/",corpus,".topredict.csv",sep=""))
}
  
  

stepTwo<-function() {
  d<-read.csv("../data/classification/request_input_tokens.csv")
  d$Text<-as.character(d$Text)
  d<-d[d$Text=="",]
  d<-tibble(line=1:nrow(d),text=d) %>% unnest_tokens(word,text)%>%group_by(line,word)%>%summarise(count=1)
  return(d)
  #write.csv(as.matrix(cast_sparse(line,text,count)),row.names=FALSE)
}



