library(XML)
library(stringr)
setwd("GTA cloud")
rm(list = ls())

## helpful case
# https://ted.europa.eu/udl?uri=TED:NOTICE:209631-2018:TEXT:EN:HTML&src=0#id4-V.

## function
get_attr=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x))), collapse=";")
}

get_value=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x))), collapse=";")
}

all.xml=list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)

# initialise
relevance=data.frame(file=character(), date=character(), directive=character(), gpa=character(),
                    value.tag=character(), value.cur=character(), value.value=character(), 
                    cpv.tag=character(), cpv.value=character(), stringsAsFactors = F)

# loop
step=0
n=length(all.xml)

while(length(all.xml)>0){
  doc=xmlParse(all.xml[1])
  file=all.xml[1]
  
  ## directive
  dir=NA
  if(length(xmlElementsByTagName(xmlRoot(doc), "DIRECTIVE", recursive = T)) >0){
    dir=get_attr(doc, "DIRECTIVE")
  }
  
  # date
  dt=NA
  if(length(xmlElementsByTagName(xmlRoot(doc), "DATE_PUB", recursive = T)) >0){
    dt=get_value(doc, "DATE_PUB")
  }
  
  ## GPA
  g=NA
  if(length(xmlElementsByTagName(xmlRoot(doc), "CONTRACT_COVERED_GPA", recursive = T))>0){
    
    g=get_attr(doc, "CONTRACT_COVERED_GPA")
  } else{
    
    if(length(xmlElementsByTagName(xmlRoot(doc), "NO_CONTRACT_COVERED_GPA", recursive = T))>0){
      
      g="NO"
    }
    
    if(length(xmlElementsByTagName(xmlRoot(doc), "YES_CONTRACT_COVERED_GPA", recursive = T))>0){
      
      g="YES"
    }
    
  } 
  
  ## CPV
  c.tag=NA
  cpv=NA
  
  if(length(xmlElementsByTagName(xmlRoot(doc), "CPV_CODE", recursive = T))>0){
    c.tag="CPV_CODE"
    cpv=get_attr(doc, "CPV_CODE")
  } else {
    if(length(xmlElementsByTagName(xmlRoot(doc), "ORIGINAL_CPV", recursive = T))>0){
      c.tag="ORIGINAL_CPV"
      cpv=get_attr(doc, "ORIGINAL_CPV")
    }
  }
  
  ## value
  vls=c("VALUE", "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", "VAL_TOTAL", "INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT", "COSTS_RANGE_AND_CURRENCY", "VAL_ESTIMATED_TOTAL", "VAL_OBJECT", "VAL_TOTAL_AFTER", "VAL_TOTAL_BEFORE", "DOCUMENT_COST", "PRICE_PAID", "EXCLUDING_VAT_VALUE", "VAL_RANGE_TOTAL", "VAL_BARGAIN_PURCHASE", "VALUE_RANGE", "VAL_SUBCONTRACTING", "VAL_PRIZE", "PRIZE_VALUE")
  
  v.tag=NA
  v.value=NA
  v.c=NA
  for(v in vls){
    
    if(length(xmlElementsByTagName(xmlRoot(doc), v, recursive = T))>0){
      v.tag=v
      v.c=get_attr(doc, v)
      v.value=get_value(doc, v)
    }
    
  }
  
  
  relevance=rbind(relevance, data.frame(file=file,
                                        gpa=g,
                                        directive=dir,
                                        date=dt,
                                        value.tag=v.tag, 
                                        value.cur=v.c,
                                        value.value=v.value, 
                                        cpv.tag=c.tag, 
                                        cpv.value=cpv))
  
  print((n-length(all.xml))/n)
  step=step+1
  if(step>=1000){
    load("22 TED scrape/result/relevance.Rdata")
    relevance.all=rbind(relevance.all, relevance)
    save(relevance.all, file="22 TED scrape/result/relevance.Rdata")
    
    relevance=data.frame(file=character(), directive=character(),
                         value.tag=character(), value.value=character(), 
                         cpv.tag=character(), cpv.value=character(), stringsAsFactors = F)
    
    
    rm(relevance.all)
    step=0
  }
  all.xml=all.xml[-1]
  print(step)
  print(nrow(relevance))
}

load("22 TED scrape/result/relevance.Rdata")
relevance=rbind(relevance.all, relevance)
save(relevance.all, file="22 TED scrape/result/relevance.Rdata")
