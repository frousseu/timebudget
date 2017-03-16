
library(data.table)
request<-function(name,subject=NA,date=NA,time=NA,date_meet=NA,comment=NA){
  r<-fread("requests.csv",showProgress=FALSE,na="",encoding="UTF-8")
  if(is.na(subject)){
    w<-max(which(r[,"name"]==name))
    if(!is.na(as.data.frame(r)[w,"date_meet"])){
      stop("A meeting date already exists")
    }else{
      r[w,"date_meet"]<-date_meet
      r[w,"comment"]<-comment
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

request(name="AMackay",date_meet="2017-01-20")
request(name="SEngelhardt",subject="R coding",date="2017-01-20",time="16:00:00")
request(name="ILaigle",subject="random effects",date="2017-01-20",time="13:30:00",date_meet="2017-01-20")
request(name="DScott",subject="response variable linear models",date="2017-01-20",time="13:30:30",date_meet="2017-01-20")
request(name="SYoga-Bengbate",sub="non-linear mixed models",date="2017-01-21",time="21:30:00")
request(name="MBelluau",date_meet="2017-01-24")  
request(name="CGeoffroy",sub="Warblers")
request(name="SYoga-Bengbate",date_meet="2017-01-25")
request(name="CGeoffroy",date_meet="2017-01-25")
request(name="DScott",date_meet="2017-01-31")
request(name="CCôté-Germain",sub="modèles mixtes et résidus")
request(name="GMoreau",date_meet="2017-02-01")
request(name="SEngelhardt",sub="further genealogy coding")
request(name="L-ARenaud",sub="bayesian interval censoring",time="13:30:00")
request(name="IDrobyshev",sub="tree rings non-linearity",date="2017-02-07",time="23:25:00")
request(name="RMarkgraf",sub="permanova",date="2017-02-08",time="15:47:00",comment="quick email to me and JA and SD")
request(name="GLetendre",sub="analyses stats",date="2017-02-11",time="10:15:00",comment="crédits de recherche avec MVellend")
request(name="RMarkgraf",sub="multiple comparisons",time="13:00:00",date_meet="2017-02-13")
request(name="GLetendre",date_meet="2017-02-17")
request(name="ILaigle",subject="model assumptions",date="2017-02-20",time="15:30:00",date_meet="2017-02-22")
request(name="AAguilar-Melo",subject="graphics interactions")
request(name="JTremblay",subject="radar maps",date="2017-02-27",time="13:30:00",date_meet="2017-02-27")
request(name="SEngelhardt",subject="more reproductive overlap and order",date="2017-02-15",time="08:53:00",date_meet="2017-02-15")
request(name="AAguilar-Melo",date_meet="2017-03-01",comment="simple ratio index response variable, visreg legend")
request(name="RBradley",date_meet="2017-03-01",comment="ideally within 4 weeks")
request(name="IDrobyshev",date_meet="2017-02-20",comment="actually burned area influence subsequent burned area")
request(name="IDrobyshev",sub="glmer models interpretation")
request(name="NTran",sub="github rstudio workflow",time="10:53:00")
request(name="M-APoirier",sub="random permutations",time="13:30:00")
request(name="ELefol",sub="ICC repeatability",time="15:00:00",date_meet="2017-03-14")
request(name="JAllostry",sub="moustiques intro stats R",date="2017-03-15",time="19:00:00")

  
  
  