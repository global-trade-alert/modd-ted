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


load("22 TED scrape/result/TED GTA-relevant cases.Rdata")
load("22 TED scrape/result/TED processed cases.Rdata")


final=unique(subset(ted.gta.base, id %in% subset(ted.processed, fully.parsed==F)$id)[,c("id","file", "gpa","directive", "date","cpv.value")])


if(nrow(final)>0){
  
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
    
    
    
    
    
    
    
    
    print(i/nrow(final))
    rm(multi, en.present)
    rm(multi.url, en.url.present)
    
    step=step+1
    if(step>=500){
      save(final, ted.gta.value, ted.gta.cpv, ted.gta.currencies, file=paste("22 TED scrape/result/Final data set - ", Sys.Date(),".Rdata", sep=""))
      step=0
    }  
  }
  save(final, ted.gta.value, ted.gta.cpv, ted.gta.currencies, file=paste("22 TED scrape/result/Final data set - ", Sys.Date(),".Rdata", sep=""))
  save(final, ted.gta.value, ted.gta.cpv, ted.gta.currencies, file=paste("22 TED scrape/result/Final data set.Rdata", sep=""))
  
  
  remDr$delete()
  
  
  ## tender number
  final$tender.number=gsub("_","-", str_extract(final$file,"\\d+_20\\d{2}"))
  
  ## cleaning directive
  final$directive=as.character(final$directive)
  final$directive[final$directive=="2014/25/EU;2014/25/EU"]="2014/25/EU"
  unique(final$directive)
  
  
  
  

  
  ## Storing into final file
  load("22 TED scrape/result/GTA-TED data set.Rdata")
  final.gta.base=rbind(final.gta.base, final)
  final.gta.value=rbind(final.gta.value, ted.gta.value)
  final.gta.cpv=rbind(final.gta.cpv, ted.gta.cpv)
  final.gta.currencies=rbind(final.gta.currencies, ted.gta.currencies)
  
  
  save(final.gta.base, final.gta.value, final.gta.cpv, final.gta.currencies, file=paste("22 TED scrape/result/GTA-TED data set.Rdata", sep=""))
  
  
}

  