# This code was authored by Henjin : https://sars2.net/czech.html
# And is provided simply to debunk it.

library(data.table)

# unique apply (faster for long vectors with many repeated values)
ua=\(x,y,...){u=unique(x);y(u,...)[match(x,u)]}

# fast way to get age in floored years between dates after 1900 and before 2100
age=\(x,y){class(x)=class(y)=NULL;(y-x-(y-789)%/%1461+(x-789)%/%1461)%/%365}

mindate=as.Date("2020-1-1");maxdate=as.Date("2022-12-31")

rec=fread("data/Vesely_106_202403141131.csv")
t=rec[,.(id=1:.N,death=DatumUmrti)]
set.seed(0);t$birth=ua(paste0(rec$Rok_narozeni,"-1-1"),as.Date)+sample(0:364,nrow(t),T)
t=t[rep(1:nrow(t),8),]
t$dose=rep(0:7,each=nrow(rec))
t$date=c(rep(mindate,nrow(rec)),rec[,`class<-`(unlist(.SD,,F),"Date"),.SDcols=paste0("Datum_",1:7)])
t$type=c(rep("",nrow(rec)),rec[,unlist(.SD,,F),.SDcols=paste0("OckovaciLatka_",1:7)])
t=t[!is.na(date)][date<=maxdate][order(-date)]
t$vaxmonth=ua(t$date,substr,1,7)

name1=unique(t$type);name2=rep("Other",length(name1))
name2[name1==""]=""
name2[grep("comirnaty",ignore.case=T,name1)]="Pfizer"
name2[grep("spikevax",ignore.case=T,name1)]="Moderna"
name2[grep("nuvaxovid",ignore.case=T,name1)]="Novavax"
name2[name1=="COVID-19 Vaccine Janssen"]="Janssen"
name2[name1=="VAXZEVRIA"]="AstraZeneca"
t$type=name2[match(t$type,name1)]

rm(rec) # free up memory so the script won't have to use swap

buck=data.table()
for(day in as.list(seq(mindate,maxdate,1))){
  cat(as.character(day),"\n")
  buck=rbind(buck,unique(t[date<=day&(is.na(death)|day<=death)&day>=birth],by="id")[ # remove under earlier doses
    # buck=rbind(buck,t[date<=day&(is.na(death)|day<=death)&day>=birth][!(dose==0&id%in%id[dose>0])][ # keep under earlier doses
    ,.(month=substr(day,1,7),vaxmonth,week=ifelse(type=="",0,as.numeric(day-date)%/%7),
       age=age(birth,day),dose,type,alive=1,dead=death==day)])[,.(
         alive=sum(alive),dead=sum(dead,na.rm=T)),by=.(month,vaxmonth,week,age,dose,type)]
}

setorder(buck,month,vaxmonth,week,age,dose,type)
fwrite(buck,"czbuckets2.csv",quote=F)