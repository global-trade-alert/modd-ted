library(stringr)
library(XML)
library(stringr)
library(splitstackshape)

rm(list = ls())
setwd("Dropbox/GTA cloud")
temp.directory="../../ted"


## Extracting zips
## rerun: pub.dates="2011-11"
pub.dates=c(paste(2011,sprintf("%02i",c(1:10)), sep="-"))


## restricting the zips
all.zips=list.files(path = "8 Data dumps/22 TED scrape/data", pattern = ".tar.gz",  full.names = T)
all.zips=all.zips[grepl(paste(pub.dates, collapse="|"), all.zips)]
all.zips=all.zips[length(all.zips):1]


##2011-10

all.zips=all.zips[2:10]
## extracting the relevant XML files
count=0
for(zip in all.zips){
  
  ## cleaning out the temp folder from XML files
  old.files=list.files(path = temp.directory,  full.names = T, recursive = T)
  unlink(old.files)
  rm(old.files)
  
  
  print(paste(zip, "unzip"))
  
  untar(zip,exdir=temp.directory)
  
  sub.tar=list.files(temp.directory, pattern = ".tar.gz", full.names = TRUE)
  if(length(sub.tar)>0){
    for(i in 1:length(sub.tar)){
      untar(sub.tar[i],exdir=temp.directory)
      print(sub.tar[i])
    }
  }
  print(zip)
  unlink(paste(temp.directory,"/*.tar.gz",sep=""),recursive = T)
  
  
  
  all.files=list.files(path = temp.directory, pattern = ".xml",  full.names = T, recursive = T)
  
  ## classifiying for relevance
  print(paste(zip, "classifying"))
  
  files.checked=0
  tryCatch({
  for(check.file in all.files){
    
    if(any(grepl("gpa", names(xmlElementSummary(check.file)$nodeCounts), ignore.case = T))){
      
      xtree <- xmlInternalTreeParse(check.file)
      root <- xmlRoot(xtree)
      
      gpa.tag=names(xmlElementSummary(check.file)$nodeCounts)[grepl("gpa", names(xmlElementSummary(check.file)$nodeCounts), ignore.case = T)]
      
      
      print(capture.output(xmlElementsByTagName(root, gpa.tag, recursive = T)))
      
      if(any(grepl('\\"NO',capture.output(xmlElementsByTagName(root, gpa.tag, recursive = T)))) & 
         any(grepl("directive", names(xmlElementSummary(check.file)$nodeCounts), ignore.case = T))){
        
        
        directive.tag=names(xmlElementSummary(check.file)$nodeCounts)[grepl("directive", names(xmlElementSummary(check.file)$nodeCounts), ignore.case = T)]
        
        print(capture.output(xmlElementsByTagName(root, directive.tag, recursive = T)))
        
        if(any(grepl("(2004/17)|(2014/25)",
                   capture.output(xmlElementsByTagName(root, directive.tag, recursive = T))))){
        
          
          store.path=paste("8 Data dumps/22 TED scrape/relevant cases/pre-2014ish/",str_extract(check.file, "\\d+.\\d+\\.xml"),sep="")
          
          file.copy(check.file,
                    store.path, overwrite = T)
          count=count+1
          print(paste("FOUND ", count))
          
          }
        
      }
      

      
 
      files.checked=files.checked+1
      print(round(files.checked/length(all.files),4))
    }
    print(paste(zip, check.file, sep="--------"))
  }
  })
  
}

