library(XML)
library(stringr)
library(splitstackshape)
library(IMFData)
library(webdriver)
library(gtalibrary)
gta_setwd()
rm(list = ls())


## the list of files to parse
load("8 Data dumps/22 TED scrape/log/TED log.Rdata")


relevant.files=subset(ted.log, file.extracted==T & fully.parsed==F)$file


new.xml=list.files(path = "8 Data dumps/22 TED scrape/relevant cases", pattern = ".xml",  full.names = T, recursive = T)
new.xml=new.xml[new.xml %in% relevant.files]

## loading data
gta.ted.db.path="8 Data dumps/22 TED scrape/result/GTA TED database.Rdata"
load(gta.ted.db.path)

xml.start=paste(min(as.numeric(gsub("\\D+","",unique(unlist(str_extract_all(new.xml, "\\d+.xml$")))))), "-01-01",sep="")
xml.end=paste(max(as.numeric(gsub("\\D+","",unique(unlist(str_extract_all(new.xml, "\\d+.xml$")))))), "-12-31",sep="")

fx.rate=gta_get_imf_data(start.date=xml.start,
                         end.date=xml.end,
                         frequency='M',
                         series='fx',
                         countries='all')


while(length(new.xml)>0){
  ## parsing the file
  file=str_extract(new.xml[1], "\\d+_\\d+\\.xml")
  print(paste("parsing file" ,file))  
  xtree <- xmlInternalTreeParse(new.xml[1])
  root <- xmlRoot(xtree)
  
  ted.parsed=gta_collect_dom(root,1)
  ted.parsed$file=file
  
  parsing.result=gta_ted_parse(dom.df=ted.parsed,
                               fx.df=fx.rate)
  
  ted.log$fully.parsed[ted.log$file==file]=parsing.result$parse.successful
  
  if(parsing.result$parse.successful){
    ted.base=parsing.result$ted.base
    ted.base$file=file
    
    ted.value=parsing.result$ted.value
    ted.value$file=file
    
    ted.cpv=data.frame(file=file,
                       cpv.code=parsing.result$ted.cpv,
                       stringsAsFactors = F)
    
    ted.gta.base=rbind(ted.gta.base, 
                       ted.base)
    
    ted.gta.value=rbind(ted.gta.value, 
                        ted.value)
    
    ted.gta.cpv=rbind(ted.gta.cpv, 
                       ted.cpv)
    
    save(ted.gta.base,ted.gta.value, ted.gta.cpv, file=gta.ted.db.path)
    rm(ted.base, ted.cpv,ted.value)
  }
  
  rm(parsing.result)
  
  ## storing
  new.xml=new.xml[-1]
  save(ted.log, "8 Data dumps/22 TED scrape/log/TED log.Rdata")
}

