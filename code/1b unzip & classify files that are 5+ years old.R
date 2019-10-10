library(stringr)
library(XML)
library(stringr)
library(splitstackshape)

rm(list = ls())
gtalibrary::gta_setwd()

## function
get_attr=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x))), collapse=";")
}

get_value=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x))), collapse=";")
}


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
                     data.frame(file=check.file,
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
                       data.frame(file=check.file,
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


## Extracting zips
pub.dates=c(paste(2014,sprintf("%02i",c(1:4)), sep="-"),
            paste(2013,sprintf("%02i",c(1:12)), sep="-"),
            paste(2012,sprintf("%02i",c(1:12)), sep="-"),
            paste(2011,sprintf("%02i",c(1:12)), sep="-"))


## restricting the zips
all.zips=list.files(path = "8 Data dumps/22 TED scrape/data", pattern = ".tar.gz",  full.names = T)
all.zips=all.zips[grepl(paste(pub.dates, collapse="|"), all.zips)]
all.zips=all.zips[length(all.zips):1]


## extracting the relevant XML files
for(zip in all.zips){
  
  ## cleaning out the temp folder from XML files
  old.files=list.files(path = "8 Data dumps/22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
  unlink(old.files)
  rm(old.files)
  
  
  print(paste(zip, "unzip"))
  
  untar(zip,exdir="8 Data dumps/22 TED scrape/temp")
  
  sub.tar=list.files("8 Data dumps/22 TED scrape/temp", pattern = ".tar.gz", full.names = TRUE)
  if(length(sub.tar)>0){
    for(i in 1:length(sub.tar)){
      untar(sub.tar[i],exdir="8 Data dumps/22 TED scrape/temp")
      print(sub.tar[i])
    }
  }
  print(zip)
  unlink("8 Data dumps/22 TED scrape/temp/*.tar.gz",recursive = T)
  
  
  
  all.files=list.files(path = "8 Data dumps/22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
  
  ## classifiying for relevance
  print(paste(zip, "classifying"))
  
  kroepfchen=c()
  r=data.frame()
  ted.parsed.complete=data.frame()
  
  step=0
  relevant.cases=0
  batch=1
  for(check.file in all.files){
    step=step+1
    
    
    xtree <- xmlInternalTreeParse(check.file)
    root <- xmlRoot(xtree)
    
    file=str_extract(check.file, "\\d+_\\d+\\.xml")
    ted.parsed=data.frame()
    
    gta_collect_dom(root,1)
    
    if(any(grepl("gpa", ted.parsed$element.name, ignore.case = T))){
      stop("found one")
    }
    
    ted.parsed.complete=rbind(ted.parsed.complete,
                              subset(ted.parsed, grepl("^[a-z]+?", element.name)==F))
    
    
    ## checking for elegibiltiy
    ## utilities directive
    dir.in=F
    
    directive=character()
    # dir.pos=ted.parsed$position[ted.parsed$element.name=="LEGAL_BASIS"]
    # if(length(dir.pos)>0){
    #   directive=paste(unique(ted.parsed$element.value[ted.parsed$position %in% dir.pos & ted.parsed$is.attribute==T]  ), collapse=";")
    # } else {
    #   
    #   dir.pos=ted.parsed$position[ted.parsed$element.name=="DIRECTIVE"]
    #   if(length(dir.pos)>0){
    #     directive=paste(unique(ted.parsed$element.value[ted.parsed$position %in% dir.pos & ted.parsed$is.attribute==T]  ), collapse=";")
    #   }
    #   
    #   
    # }
    # 
    
    ## directive
    directive=character()
    if(length(xmlElementsByTagName(xmlRoot(doc), "DIRECTIVE", recursive = T)) >0){
      directive=get_attr(doc, "DIRECTIVE")
    }
    
    
    if(length(directive)==0){
      if(length(xmlElementsByTagName(xmlRoot(doc), "LEGAL_BASIS", recursive = T)) >0){
        directive=get_attr(doc, "LEGAL_BASIS")
      }
    }
    
    
    if(length(directive)==0){
      directive=NA
      
      load("8 Data dumps/22 TED scrape/result/TED pre-2014 parsing error log.Rdata")
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=check.file,
                                     issue="No directive",
                                     batch=zip,
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="8 Data dumps/22 TED scrape/result/TED pre-2014 parsing error log.Rdata")
      
      rm(ted.error.log)
      print("No legal base")
    } else {
      
      print("YES legal base")
      
     dir.in=((grepl("2004", directive) & grepl("17", directive))|(grepl("2014", directive) & grepl("25", directive)) )
    }
    
    ## contract type
    nc.in=F
    
    nc=character()
    # nc.pos=ted.parsed$position[ted.parsed$element.name=="NC_CONTRACT_NATURE"]
    # 
    # if(length(nc.pos)>0){
    #   nc=paste(unique(ted.parsed$element.value[ted.parsed$position %in% nc.pos & ted.parsed$is.attribute==T]), collapse=";")
    # }
    
    if(length(nc)==0){
      if(length(xmlElementsByTagName(xmlRoot(doc), "NC_CONTRACT_NATURE", recursive = T)) >0){
        nc=get_attr(doc, "NC_CONTRACT_NATURE")
      }
    }
    
    
    if(length(nc)==0){
      
      load("8 Data dumps/22 TED scrape/result/TED pre-2014 parsing error log.Rdata")
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=check.file,
                                     issue="No contract type",
                                     batch=zip,
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="8 Data dumps/22 TED scrape/result/TED pre-2014 parsing error log.Rdata")
      
      rm(ted.error.log)
      
    } else {
      
      nc.in=sum(as.numeric(c("2","9","Z","3") %in% nc))>0
      
    }
    
    
    ## good information from good files
    if(nc.in==T & dir.in==T){
      xtree <- xmlInternalTreeParse(check.file)
      root <- xmlRoot(xtree)
      
      ted.parsed=data.frame()
      gta_collect_dom(root,1)
      
    
      ## case.id & file
      f=str_extract(check.file, "\\d+_\\d+.xml")
      c.i=str_extract(gsub("_","-",c.i), "\\d+_\\d+")
      
      ## URL
      url=ted.parsed$element.value[ted.parsed$element.name=="URI_DOC"]
      
      if(length(url)==0){
        url=NA
      }
      
      
      ## country.code
      cc.pos=ted.parsed$position[ted.parsed$element.name=="ISO_COUNTRY"]
      cc=paste(unique(ted.parsed$element.value[ted.parsed$position %in% dir.pos & ted.parsed$is.attribute==T]  ), collapse=";")
      
      if(length(cc)==0){
        cc=NA
      }
      
      
      ## publication date
      p.date=ted.parsed$element.value[ted.parsed$element.name=="DATE_PUB"]
      
      if(length(p.date)==0){
        p.date=NA
      }
      
      
      ## storing the good stuff
      relevant.cases=relevant.cases+1
      r=rbind(r,
              data.frame(case.id=c.i,
                         url=url,
                         description=NA,
                         country.code=cc,
                         publication.date=p.date,
                         file=f,
                         file=all.files,
                         stringsAsFactors = F))
      
      ted.parsed.complete=rbind(ted.parsed.complete,
                                subset(ted.parsed, grepl("^[a-z]+?", element.name)==F))
      
    } else {
      kroepfchen=c(kroepfchen, check.file)
    }

    
    if(step%%500==0){
      print(paste(zip, "classified",step,"out of",length(all.files)))
      step=0
    }
    
    if(relevant.cases>=2000){
      ## saving the cases
      load("8 Data dumps/22 TED scrape/result/TED results - latest search.Rdata")
      r$gpa=NA
      r$collection.date=Sys.Date()
      r$file.extracted=F
      r$fully.parsed=F
      r$fully.hs.converted=F
      r$added.to.bulk.nr=NA
      r$gta.relevant=NA
      r$issuer.name=NA
      r$issuer.town=NA
      r$issuer.country=NA
      r$tender.source=NA
      r$multi.doc=NA
      
      results=rbind(results, r)
      
      save(results, file="8 Data dumps/22 TED scrape/result/TED results - latest search.Rdata")
      rm(results)
      r<<-data.frame()
      
      
      
      # saving parsing results
      parse.file=paste("8 Data dumps/22 TED scrape/result/parsed-not-processed/TED full parse - ",Sys.Date()," - batch ",batch,".Rdata",sep="")
      save(ted.parsed.complete, file=parse.file)
      save(results, file="8 Data dumps/22 TED scrape/result/TED results - latest search.Rdata")
      # reset
      relevant.cases =0
      batch = batch+1
      ted.parsed.complete<<-data.frame()
      
    }
    
    
  }
  
  ## storing the values
  load("8 Data dumps/22 TED scrape/result/TED results - latest search.Rdata")
  r$gpa=NA
  r$collection.date=Sys.Date()
  r$file.extracted=F
  r$fully.parsed=F
  r$fully.hs.converted=F
  r$added.to.bulk.nr=NA
  r$gta.relevant=NA
  r$issuer.name=NA
  r$issuer.town=NA
  r$issuer.country=NA
  r$tender.source=NA
  r$multi.doc=NA
  
  results=rbind(results, r)
  
  save(results, file="8 Data dumps/22 TED scrape/result/TED results - latest search.Rdata")
  rm(results)
  r<<-data.frame()
  
  
  
  # saving parsing results
  parse.file=paste("8 Data dumps/22 TED scrape/result/parsed-not-processed/TED full parse - ",Sys.Date()," - batch ",batch,".Rdata",sep="")
  save(ted.parsed.complete, file=parse.file)
  save(results, file="8 Data dumps/22 TED scrape/result/TED results - latest search.Rdata")
  
 
  
  ## deleting the bad stuff
  print(paste(zip, "removing the bad stuff"))
  unlink(kroepfchen)
  rm(all.files)
  
  ## Storing the good stuff
  
  all.files=list.files(path = "8 Data dumps/22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
  
  new.location=paste("8 Data dumps/22 TED scrape/relevant cases/",unlist(str_extract_all(all.files, "\\d+_\\d+.xml$")),sep="")
  
  # copying them over
  print(paste(zip, "copying relevant XMLs"))
  for(i in 1:length(new.location)){
    file.copy(from=all.files[i], to=new.location[i], overwrite = T)
    
    
    print(i/length(new.location))
  }
  
  
  # cleaning up
  print(paste(zip, "cleaning up"))
  do.call(file.remove, list(list.files("8 Data dumps/22 TED scrape/temp", full.names = TRUE)))
  
  print(paste(zip, "concluded"))
}

