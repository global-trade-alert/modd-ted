library(XML)
library(stringr)
setwd("GTA cloud")
rm(list = ls())

## helpful case
# https://ted.europa.eu/udl?uri=TED:NOTICE:209631-2018:TEXT:EN:HTML&src=0#id4-V.


## my function
tag_names=function(node){
  
  if(exists("xml.df")==F){
    xml.df=data.frame(file=character(), tag=character(), attribute=character(), stringsAsFactors = F)
  }
  num.children = xmlSize(node)  
  
  
  ## for those with children, record name and go deeper
  if(num.children > 0 ) {
    # xml.liste=append(xml.liste,xmlName(node))
    # 
    # 
    if(length(names(xmlAttrs(node)))>0){
      xml.df=rbind(xml.df, data.frame(file=file, tag=xmlName(node), attribute=names(xmlAttrs(node))))
    } else {
      xml.df=rbind(xml.df, data.frame(file=file, tag=xmlName(node), attribute=NA))
    }
    
    
    #Go one level deeper
    for (j in 1 : num.children) {
      xml.df=rbind(subset(xml.df, is.na(file)==F), tag_names(node[[j]]))
      
    }
    xml.df=unique(xml.df)
    row.names(xml.df)=NULL
    return(xml.df)
    rm(xml.df)
  }
  
}


all.xml=list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)

# initialise
xml.tags=data.frame(file=character(), tag=character(), attribute=character(), stringsAsFactors = F)

# loop
step=0
n=length(all.xml)
while(length(all.xml)>0){
  i=sample(1:length(all.xml),1)
  doc=xmlParse(all.xml[i])
  file=all.xml[i]
  xml.tags=rbind(xml.tags,
                 tag_names(xmlRoot(doc)))
  
  print((n-length(all.xml))/n)
  step=step+1
  if(step>=1000){
    load("22 TED scrape/result/xml tags.Rdata")
    result.xml.tags=rbind(result.xml.tags, xml.tags)
    save(result.xml.tags, file="22 TED scrape/result/xml tags.Rdata")
    xml.tags=data.frame(file=character(), tag=character(), attribute=character(), stringsAsFactors = F)
    rm(result.xml.tags)
    step=0
  }
  all.xml=all.xml[-i]
}

load("22 TED scrape/result/xml tags.Rdata")
result.xml.tags=rbind(result.xml.tags, xml.tags)
save(result.xml.tags, file="22 TED scrape/result/xml tags.Rdata")
xml.tags=data.frame(file=character(), tag=character(), attribute=character(), stringsAsFactors = F)



