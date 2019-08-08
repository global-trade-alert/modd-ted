library(XML)
library(stringr)
setwd("GTA cloud")
rm(list = ls())
setwd("GTA cloud")

## helpful case
# https://ted.europa.eu/udl?uri=TED:NOTICE:209631-2018:TEXT:EN:HTML&src=0#id4-V.

all.xml=list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
  
## results table
load("22 TED scrape/result/TED xml.Rdata")
# results=data.frame(id=NA,
#                    url=NA,
#                    description=NA,
#                    issuer.name=NA,
#                    issuer.country=NA,
#                    issuer.nuts=NA,
#                    award.date=NA,
#                    publication.date=NA,
#                    value.number=NA,
#                    value.currency=NA,
#                    cpv.main=NA,
#                    cpv.additional=NA,
#                    tenders.total=NA,
#                    tenders.eu=NA,
#                    tenders.noneu=NA,
#                    gpa=NA,
#                    performance.place.code=NA,
#                    recipient.name=NA,
#                    recipient.nuts.code=NA,
#                    recipient.sme=NA)

ti=Sys.time()
step=0
for(i in 1:length(all.xml)){
  #doc=xmlParse("22 TED scrape/temp/20180516_092/209631_2018.xml")
  doc=xmlParse(all.xml[i])
  doc.list=xmlToList(doc)
  
  if(!doc.list$.attrs["DOC_ID"][1] %in% results$id){
    if(length(doc.list$CODED_DATA_SECTION$CODIF_DATA$DIRECTIVE[["VALUE"]])>0){
      if(doc.list$CODED_DATA_SECTION$CODIF_DATA$DIRECTIVE[["VALUE"]] %in% c("2004/17/EC","2014/25/EU")){
      
      
      eval(parse(text=paste("my.xml=doc.list$FORM_SECTION[which(unlist(lapply(doc.list$FORM_SECTION, function(x) lapply(x['.attrs'], function(y) y['LG'])))=='EN')]$",names(doc.list$FORM_SECTION)[which(grepl("_2014",names(doc.list$FORM_SECTION)))],sep="")))
      lg="EN"
      
      if(is.null(my.xml)){
        eval(parse(text=paste("my.xml=doc.list$FORM_SECTION[1]$",names(doc.list$FORM_SECTION)[which(grepl("_2014",names(doc.list$FORM_SECTION)))],sep="")))
        eval(parse(text=paste("lg=doc.list$FORM_SECTION[1]$",names(doc.list$FORM_SECTION)[which(grepl("_2014",names(doc.list$FORM_SECTION)))],"$.attrs['LG']",sep="")))
       
      }
      
      
      ## starting values
      link.text=paste("TEXT:",lg,":HTML", sep="")
      id=NA
      url=NA
      desc=NA
      i.name=NA
      i.cty=NA
      i.nuts=NA
      a.date=NA
      p.date=NA
      v.nr=NA
      v.c=NA
      c.m=NA
      c.a=NA
      t.t=NA
      t.eu=NA
      t.neu=NA
      gpa=NA
      p.p=NA
      r.n=NA
      r.nuts=NA
      r.sme=NA
      
      ## filling gaps
      if(length(doc.list$.attrs["DOC_ID"][1])>0){
        id=doc.list$.attrs["DOC_ID"][1]
      }
      
      if(length(gsub("TEXT:..:HTML",link.text,doc.list$CODED_DATA_SECTION$NOTICE_DATA$URI_LIST$URI_DOC$text))>0){
        url=gsub("TEXT:..:HTML",link.text,doc.list$CODED_DATA_SECTION$NOTICE_DATA$URI_LIST$URI_DOC$text)
      }
      
      if(length(my.xml$OBJECT_CONTRACT$TITLE$P)>0){
        if(length(my.xml$OBJECT_CONTRACT$TITLE$P)>1){
          desc=my.xml$OBJECT_CONTRACT$TITLE$P$text
        } else {
          desc=my.xml$OBJECT_CONTRACT$TITLE$P
        }
        
      }
      
      if(is.list(desc)){
        desc=desc$FT$text
      }
      
      if(length(my.xml$CONTRACTING_BODY$ADDRESS_CONTRACTING_BODY$OFFICIALNAME)>0){
        i.name=my.xml$CONTRACTING_BODY$ADDRESS_CONTRACTING_BODY$OFFICIALNAME
      }
      
      if(length(my.xml$CONTRACTING_BODY$ADDRESS_CONTRACTING_BODY$COUNTRY["VALUE"])>0){
        i.cty=my.xml$CONTRACTING_BODY$ADDRESS_CONTRACTING_BODY$COUNTRY["VALUE"]
      }
      
      
      if(length(my.xml$CONTRACTING_BODY$ADDRESS_CONTRACTING_BODY$NUTS["CODE"])>0){
        i.nuts=my.xml$CONTRACTING_BODY$ADDRESS_CONTRACTING_BODY$NUTS["CODE"]
      }
      
      if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$DATE_CONCLUSION_CONTRACT)>0){
        a.date=as.Date(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$DATE_CONCLUSION_CONTRACT , "%Y%m%d")
      }
      
      if(length(doc.list$CODED_DATA_SECTION$REF_OJS$DATE_PUB)>0){
        p.date=as.Date(doc.list$CODED_DATA_SECTION$REF_OJS$DATE_PUB, "%Y%m%d")
      }
      
      if(length(my.xml$OBJECT_CONTRACT$VAL_ESTIMATED_TOTAL$text)>0){
        v.nr=my.xml$OBJECT_CONTRACT$VAL_ESTIMATED_TOTAL$text
      } else {
        if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$VALUE$VAL_TOTAL$text)>0){
          v.nr=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$VALUE$VAL_TOTAL$text
        }
      }
      
      if(length(my.xml$OBJECT_CONTRACT$VAL_ESTIMATED_TOTAL$.attrs["CURRENCY"])>0){
        v.c=my.xml$OBJECT_CONTRACT$VAL_ESTIMATED_TOTAL$.attrs["CURRENCY"]
      }else {
        if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$VALUE$VAL_TOTAL$.attrs["CURRENCY"])>0){
          v.c=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$VALUE$VAL_TOTAL$.attrs["CURRENCY"]
        }
      }
      
      if(length(paste(unlist(my.xml$OBJECT_CONTRACT$CPV_MAIN$CPV_CODE), collapse=";"))>0){
        c.m=paste(unlist(my.xml$OBJECT_CONTRACT[which(names(my.xml$OBJECT_CONTRACT)=="CPV_MAIN")]), collapse=";")
      }
      
      if(length(paste(unlist(my.xml$OBJECT_CONTRACT$OBJECT_DESCR$CPV_ADDITIONAL$CPV_CODE), collapse=";"))>0){
        c.a=paste(unlist(my.xml$OBJECT_CONTRACT$OBJECT_DESCR[which(names(my.xml$OBJECT_CONTRACT$OBJECT_DESCR)=="CPV_ADDITIONAL")]), collapse=";")
      }
      
      if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$TENDERS$NB_TENDERS_RECEIVED)>0){
        t.t=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$TENDERS$NB_TENDERS_RECEIVED
      }
      
      if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$TENDERS$NB_TENDERS_RECEIVED_OTHER_EU)>0){
        t.eu=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$TENDERS$NB_TENDERS_RECEIVED_OTHER_EU
      }
      
      if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$TENDERS$NB_TENDERS_RECEIVED_NON_EU)>0){
        t.neu=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$TENDERS$NB_TENDERS_RECEIVED_NON_EU
      }
      
      if(length(as.numeric(! "NO_CONTRACT_COVERED_GPA" %in% names(my.xml$PROCEDURE)))>0){
        gpa=as.numeric(! "NO_CONTRACT_COVERED_GPA" %in% names(my.xml$PROCEDURE))
      }
      
      if(length(my.xml$OBJECT_CONTRACT$OBJECT_DESCR$NUTS)>0){
        p.p=my.xml$OBJECT_CONTRACT$OBJECT_DESCR$NUTS
      }
      
      if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTOR$ADDRESS_CONTRACTOR$OFFICIALNAME)>0){
        r.n=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTOR$ADDRESS_CONTRACTOR$OFFICIALNAME
      } else{
        if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTORS$CONTRACTOR$ADDRESS_CONTRACTOR$OFFICIALNAME)>0){
          r.n=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTORS$CONTRACTOR$ADDRESS_CONTRACTOR$OFFICIALNAME
        }
      }
      
      if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTOR$ADDRESS_CONTRACTOR$NUTS)>0){
        r.nuts=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTOR$ADDRESS_CONTRACTOR$NUTS
      }else{
        if(length(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTORS$CONTRACTOR$ADDRESS_CONTRACTOR$NUTS)>0){
          r.nuts=my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTORS$CONTRACTOR$ADDRESS_CONTRACTOR$NUTS
        }
      }
      
      
      
      if(length(as.numeric(! "NO_SME" %in% names(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTOR)))>0){
        r.sme=as.numeric(! "NO_SME" %in% names(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTOR))
      }else{
        if("SME" %in% names(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTORS$CONTRACTOR)){
          r.sme=as.numeric(is.null(my.xml$AWARD_CONTRACT$AWARDED_CONTRACT$CONTRACTORS$CONTRACTOR$SME)==F)
        }
      }
      
      r=data.frame(id=id,
                         url=url,
                         description=desc,
                         issuer.name=i.name,
                         issuer.country=i.cty,
                         issuer.nuts=i.nuts,
                         award.date=a.date,
                         publication.date=p.date,
                         value.number=v.nr,
                         value.currency=v.c,
                         cpv.main=c.m,
                         cpv.additional=c.a,
                         tenders.total=t.t,
                         tenders.eu=t.eu,
                         tenders.noneu=t.neu,
                         gpa=gpa,
                         performance.place.code=p.p,
                         recipient.name=r.n,
                         recipient.nuts.code=r.nuts,
                         recipient.sme=r.sme)
      
      
      if(sum(as.numeric(is.na(r)))>15){stop()}
      
      step=step+1
      results=rbind(results, r)
      
      rm(my.xml)
    } else {unlink(all.xml[i])}
    } else{unlink(all.xml[i])}
  }
  fin=ti+ as.numeric(difftime(Sys.time(),ti, unit="secs"))/i*(length(all.xml)-i)
  print(paste("Estimated completion time: ",fin, "(file number: ",i,")", sep=""))
  
  
  if(step>1000){
    save(results, file="22 TED scrape/result/TED xml.Rdata")
    step=0
  }
}
save(results, file="22 TED scrape/result/TED xml.Rdata")
