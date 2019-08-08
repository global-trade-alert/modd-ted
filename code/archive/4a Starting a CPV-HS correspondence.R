library(XML)
library(stringr)
library(splitstackshape)
library(webdriver)
library(gtalibrary)
library(data.table)
setwd("GTA cloud")
rm(list = ls())
options("scipen"=999, "digits"=4)

load("22 TED scrape/result/Relevant TED cases")

cpv.names=read.csv("22 TED scrape/help files/cpv_names.csv",sep=";")
cpv=subset(ted.cpv, id %in% ted.base$id)
setnames(cpv.names, "cpv", "cpv.value")
cpv=merge(unique(cpv[,c("cpv.value")]), cpv.names, all.x=T)
cpv$cpv.name=as.character(cpv$cpv.name)

cpv.phrases=unique(cpv$cpv.name)

cpv.checked=data.frame(cpv.value=character(),
                       code.full=character(),
                       cpv.name=character(),
                       hs.code=character(),
                       source=character(),
                       stringsAsFactors = F)


last.i=1

##HALLO PATRICK
# 
# Falls du diesen Code unterbrechen musst, drück einfach stop. 
# Lass die environment im memory und schau dass sie drin bleibt (oder speicher das image irgendwo hin).
# um neu zu starten, lass zeile 37 und den loop ab Zeile 38 laufen.

last.i=i-1 ## für Neustart
for(i in last.i:length(cpv.phrases)){
  
  query.result=gta_hs_code_finder(cpv.phrases[i], aggregate=F)
  
  if(nrow(query.result)>0){
    
    setnames(query.result,"product.name", "cpv.name")
    cpv.checked=rbind(cpv.checked,
                      merge(subset(cpv, cpv.name==cpv.phrases[i]), query.result, by="cpv.name", all.x=T))
  }

  print(paste("Concluded query #",i," out of ",length(cpv.phrases),".",sep=""))
}


save(cpv.checked, cpv, file="22 TED scrape/help files/CPC-HS correspondence.Rdata")
