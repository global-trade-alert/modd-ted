library(XML)
library(stringr)
library(splitstackshape)
library(IMFData)
library(webdriver)
setwd("GTA cloud")
rm(list = ls())

## based on 
#  https://gist.github.com/Ram-N/5189462

gta_collect_dom <- function(node=NULL, 
                      dom.pos=c()) {
  
  if (is.null(node)) {
    #leaf node reached. Turn back
    return()
  }
  # print(paste("Node: ", xmlName(node)))
  
  e.name=xmlName(node)
  e.value=xmlValue(node)
  e.type=F
  
  ted.parsed<<-rbind(ted.parsed,
                   data.frame(file=file,
                              position=dom.pos,
                              element.name=e.name,
                              is.attribute=e.type,
                              element.value=e.value, 
                              stringsAsFactors = F))
  
  if(length(xmlAttrs(node))>0){
    
    x.attr=xmlAttrs(node)
    
    e.name=names(x.attr)
    e.value=x.attr
    e.type=T
    
    ted.parsed<<-rbind(ted.parsed,
                       data.frame(file=file,
                                  position=dom.pos,
                                  element.name=e.name,
                                  is.attribute=e.type,
                                  element.value=e.value, 
                                  stringsAsFactors = F))
    rm(x.attr)
  }
  
  num.children = xmlSize(node) 
  
  #Go one level deeper
  if (num.children > 0) { 
    for (i in 1 : num.children) { 
      gta_collect_dom(node[[i]],paste(dom.pos, i, sep="")) #the i-th child of node 
    } 
  }
}



## the list of files to parse
load("22 TED scrape/result/TED results - latest search.Rdata")


results=subset(results, file.extracted==T & fully.parsed==F)
relevant.files=results$file


all.xml=list.files(path = "22 TED scrape/relevant cases", pattern = ".xml",  full.names = T, recursive = T)
all.xml=all.xml[all.xml %in% relevant.files]

## remove already parsed entries
load("22 TED scrape/result/GTA TED full parse.Rdata")
parsed.files=unique(ted.parsed.complete$file)
all.xml=all.xml[!str_extract(all.xml, "\\d+_\\d+\\.xml") %in% parsed.files]



# loop & add
step=0
batch=1

ted.parsed.complete=data.frame()

while(length(all.xml)>0){
  # doc=xmlParse(all.xml[1], useInternalNodes = F)
  # doc=xmlParse(all.xml[1])
  #read the XML tree into memory
  
  xtree <- xmlInternalTreeParse(all.xml[1])
  root <- xmlRoot(xtree)
  
  file=str_extract(all.xml[1], "\\d+_\\d+\\.xml")
  ted.parsed=data.frame()
  
  gta_collect_dom(root,1)
  
  ted.parsed.complete=rbind(ted.parsed.complete,
                            subset(ted.parsed, grepl("^[a-z]+?", element.name)==F))
  
  all.xml=all.xml[-1]
  print(length(all.xml))
  
  step=step+1
  results$fully.parsed[results$file==all.xml]=T
  
  if(step>=2000){
    
    # saving parsing results
    parse.file=paste("22 TED scrape/result/parsed-not-processed/TED full parse - ",Sys.Date()," - batch ",batch,".Rdata",sep="")
    save(ted.parsed.complete, file=parse.file)
    save(results, file="22 TED scrape/result/TED results - latest search.Rdata")
    # reset
    step =0
    batch = batch+1
    ted.parsed.complete<<-data.frame()
    
  }
}

parse.file=paste("22 TED scrape/result/parsed-not-processed/TED full parse - ",Sys.Date()," - batch ",batch,".Rdata",sep="")
save(ted.parsed.complete, file=parse.file)
save(results, file="22 TED scrape/result/TED results - latest search.Rdata")
