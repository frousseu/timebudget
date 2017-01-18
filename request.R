
library(data.table)
request<-function(name,subject=NA,date=NA,time=NA,date_meet=NA,comment=NA){
  r<-fread("requests.csv",showProgress=FALSE)
  if(is.na(subject)){
    w<-max(which(r[,"name"]==name))
    if(!is.na(r[w,"date_meet"])){
      stop("A meeting date already exists")
    }else{
      r[,"name"]<-date_meet
    }
  }else{
    ct<-Sys.time()
    if(is.na(date)){
      date<-substr(ct,1,10)
    }
    if(is.na(time)){
      time<-substr(ct,12,19)
    }
    add<-data.frame(name,subject,date,time,date_meet,comment,stringsAsFactors=FALSE)
    #g<-getURL("https://raw.githubusercontent.com/frousseu/timebudget/master/requests.csv") # Ce fichier est sur mon github
    #g<-read.csv(text=g,header=TRUE,stringsAsFactors=FALSE)
    r<-rbind(r,add)
    
  }
  r<-unique(r)
  r<-r[order(r[,"date"],r[,"time"]),]
  fwrite(r,file="requests.csv",showProgress=FALSE)
}

request("SRivest",sub="curve fitting GAM",time="12:10:00",date_meet="2017-01-16",comment="also check how to get variance")
request("NTran",sub="incubation package",time="14:20:00",date_meet="2017-01-16",comment="optimisation github packages etc.")
request("ELefol",sub="DBTREScheck",date="2017-01-12",time="10:00:00",date_meet="2017-01-16",comment="consult Nghia and Dave")
request("BShipley",sub="PathAnalysisCourse",date="2017-01-11",time="10:00:00",date_meet="2017-01-11",comment="")
request("RBradley",sub="mahalanobsis",date="2017-01-09",time="11:00:00")
request("MBélisle",sub="paper Yanick",date="2017-01-13",time="10:00:00",date_meet="2017-01-13",comment="")


  
  
  
  
  