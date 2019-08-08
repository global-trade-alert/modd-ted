library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
library(splitstackshape)
library(webdriver)
options("scipen"=999, "digits"=4)

rm(list = ls())

setwd("GTA cloud")

## functions
get_attr=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x))), collapse=";")
}

get_value=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x))), collapse=";")
}

get_attr_vector=function(document, attribute){
  unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x)))
}

get_value_vector=function(document, attribute){
  unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x)))
}


load("22 TED scrape/result/Relevant TED cases")


final=unique(ted.base[,c("id","file", "gpa","directive", "date","cpv.value")])
final$file=gsub("temp/\\d+_\\d+","relevant cases",final$file)

final$issuer.name=NA
final$issuer.town=NA
final$issuer.country=NA
final$cpv.title=NA
final$contract.tag=NA
final$contract.value=NA
final$tender.source=NA
final$multi.doc=NA

# for checking whether source site still exists
pjs <- run_phantomjs()
remDr=Session$new(port=pjs$port)

step=0
for(i in 1:nrow(final)){
  doc=xmlParse(final$file[i])
  
  # issuer name
  if(length(xmlElementsByTagName(xmlRoot(doc), "AA_NAME", recursive = T)) >0){
    if(sum(as.numeric(get_value_vector(doc, "AA_NAME")=="EN"))>0){
      final$issuer.name[i]=get_value_vector(doc, "AA_NAME")[which(get_value_vector(doc, "AA_NAME")=="EN")]
    } else {
      final$issuer.name[i]=get_value_vector(doc, "AA_NAME")[1]
    }
  }
  
  
  
  ## multi doc languages
  if(length(xmlElementsByTagName(xmlRoot(doc), "ML_TI_DOC", recursive = T)) >0){
    final$multi.doc[i]=get_attr(doc, "ML_TI_DOC")
    multi=get_attr_vector(doc, "ML_TI_DOC")
    en.present=sum(as.numeric(multi=="EN"))
  }
  
  ## Tender town
  if(length(xmlElementsByTagName(xmlRoot(doc), "TI_TOWN", recursive = T)) >0){
    if(en.present>0){
      final$issuer.town[i]=get_value_vector(doc, "TI_TOWN")[which(multi=="EN")]
    } else {
      final$issuer.town[i]=get_value(doc, "TI_TOWN")
    }
    
  }
  
  # tender country
  if(length(xmlElementsByTagName(xmlRoot(doc), "ISO_COUNTRY", recursive = T)) >0){
    final$issuer.country[i]=get_attr(doc, "ISO_COUNTRY")
  }
  
  ## CPV headline
  if(length(xmlElementsByTagName(xmlRoot(doc), "TI_TEXT", recursive = T)) >0){
    if(en.present>0){
      final$cpv.title[i]=get_value_vector(doc, "TI_TEXT")[which(multi=="EN")]
    } else {
      final$cpv.title[i]=get_value(doc, "TI_TEXT")
    }
  }
  
  # vls=c("VALUE", "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", "VAL_TOTAL", "INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT", "COSTS_RANGE_AND_CURRENCY", "VAL_ESTIMATED_TOTAL", "VAL_OBJECT", "VAL_TOTAL_AFTER", "VAL_TOTAL_BEFORE", "DOCUMENT_COST", "PRICE_PAID", "EXCLUDING_VAT_VALUE", "VAL_RANGE_TOTAL", "VAL_BARGAIN_PURCHASE", "VALUE_RANGE", "VAL_SUBCONTRACTING", "VAL_PRIZE", "PRIZE_VALUE")
  # 
  # v.tag=NA
  # v.value=NA
  # v.c=NA
  # for(v in vls){
  #   if(length(xmlElementsByTagName(xmlRoot(doc), v, recursive = T))>0){
  #     
  #     v.tag=v
  #     v.c=get_attr_vector(doc, v)
  #     v.value=get_value_vector(doc, v)
  #     v.value=paste(paste(v.c, v.value, sep=": "), sep=";")
  #   }
  #   
  # }
  # 
  # 
  # final$contract.tag[i]=v.tag
  # final$contract.value[i]=v.value
  
  
  ## tender source
  if(length(xmlElementsByTagName(xmlRoot(doc), "URI_DOC", recursive = T)) >0){
    multi.url=get_attr_vector(doc, "URI_DOC")
    en.url.present=sum(as.numeric(multi.url=="EN"))
    
    if(en.url.present>0){
      final$tender.source[i]=get_value_vector(doc, "URI_DOC")[which(multi.url=="EN")]
    } else {
      final$tender.source[i]=get_value(doc, "URI_DOC")
    }
    
    ## check sources
    remDr$go(final$tender.source[i])
    if(remDr$getUrl()=="https://ted.europa.eu/TED/error-pages/expiredUDLLink.do"){
      final$tender.source[i]="https://ted.europa.eu/TED/search/search.do"
    }
  }
  
  
  print(i/nrow(final))
  rm(multi, en.present)
  rm(multi.url, en.url.present)
  
  step=step+1
  if(step>=500){
    save(final, ted.value, ted.cpv, ted.currencies, file=paste("22 TED scrape/result/Final data set - ", Sys.Date(),".Rdata", sep=""))
    step=0
  }  
}
save(final, ted.value, ted.cpv, ted.currencies, file=paste("22 TED scrape/result/Final data set - ", Sys.Date(),".Rdata", sep=""))
save(final, ted.value, ted.cpv, ted.currencies, file=paste("22 TED scrape/result/Final data set.Rdata", sep=""))


remDr$delete()


## GTA IDs
final$gta.id=1:nrow(final)

## tender number
final$tender.number=gsub("_","-", str_extract(final$file,"\\d+_20\\d{2}"))

## cleaning directive
final$directive=as.character(final$directive)
final$directive[final$directive=="2014/25/EU;2014/25/EU"]="2014/25/EU"
unique(final$directive)


## cleaning values
ted.currencies$id=NULL
ted.currencies$date=NULL
ted.currencies=unique(ted.currencies)

ted.value=unique(final[,c("id","date", "contract.value")])
ted.value$value.cur=apply(ted.value, 1, function(x) paste(unique(unlist(str_extract_all(x[which(names(ted.value)=="contract.value")], "[A-Z]{3}"))), collapse=";"))
ted.value$y.m=paste(year(ted.value$date), month(ted.value$date), sep="-")
ted.value$y.m[month(ted.value$date)<10]=paste(year(ted.value$date[month(ted.value$date)<10]),"-0", month(ted.value$date[month(ted.value$date)<10]), sep="")
ted.value=merge(ted.value, ted.currencies, by=c("y.m", "value.cur"), all.x=T)
ted.value$value.nr=gsub("([A-Z]+: )|(: )","", ted.value$contract.value)
ted.value$value.nr=gsub(",\\d{2}",";", ted.value$value.nr)

ted.value=cSplit(ted.value, which(names(ted.value)=="value.nr"), direction="long", sep=";")

non.split=subset(ted.value, is.na(str_extract(value.nr, " \\d{4,} "))==F)
non.split$odd.values=str_extract(non.split$value.nr, " \\d{4,} ")
non.split=subset(non.split, is.na(odd.values)==F)
non.split$odd.values=gsub("(^\\d{3})(\\d+)","\\1;\\2", gsub(" ","",non.split$odd.values))
non.split$value.nr=apply(non.split, 1, function(x) gsub(" \\d{4,} ",x[which(names(non.split)=="odd.values")],x[which(names(non.split)=="value.nr")]))
non.split$odd.values=NULL
non.split=cSplit(non.split, which(names(non.split)=="value.nr"), direction="long", sep=";")
ted.value=rbind(subset(ted.value, !id %in% non.split$id),non.split)

ted.value$value.nr=as.numeric(gsub(" ","",ted.value$value.nr))
ted.value=subset(ted.value, is.na(divide.by)==F)

ted.value$value.usd=ted.value$value.nr/as.numeric(ted.value$divide.by)

min.max=merge(aggregate(value.usd ~ id, ted.value, function(x) length(unique(x))),merge(aggregate(value.usd ~ id, ted.value, min), aggregate(value.usd ~ id, ted.value, max), by="id"), by="id")
names(min.max)=c("id", "cases","min", "max")
min.max$odd=as.numeric(min.max$min<10000000 & min.max$max>10000000 & min.max$cases>1)

ted.value=subset(ted.value, id %in% subset(min.max, min>10000000)$id)
ted.value=merge(aggregate(value.usd ~id, ted.value, max), ted.value[,c("id", "value.usd","value.cur", "value.nr")], by=c("id", "value.usd"), all.x=T)

names(ted.value)=c("id", "value.usd", "lcu.unit", "value.lcu")

final=merge(final, ted.value, by="id") ## removing all those without the right values.


### ODD values
final=subset(final, !tender.number %in% c("269373-2017", "499380-2017", "179395-2017","204469-2016", "522405-2017","493261-2017", "349780-2017", "371754-2017" ,"443459-2014"))
final=subset(final, !tender.number %in% c("474378-2017", "474313-2017", "264859-2018", "150071-2016"))

final$value.lcu[final$tender.number=="190770-2014"]=800000000
final$value.usd[final$tender.number=="190770-2014"]=800000000/.62

## cleaning unicode names
final$issuer.name=gsub(">",";",gsub("(<.*?)(\\d+)","&#1\\2",enc2native(final$issuer.name)))
final$issuer.name[grepl("[0-9][A-Z];", final$issuer.name)]=tolower(final$issuer.name[grepl("[0-9][A-Z];", final$issuer.name)])
final$issuer.name=gsub(""|"","",final$issuer.name)


save(final, ted.value, ted.cpv, ted.currencies, file=paste("22 TED scrape/result/Final data set.Rdata", sep=""))

  