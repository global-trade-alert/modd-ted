library(XML)
library(httr)
library(stringr)
setwd("GTA cloud")


all.zips=list.files(path = "22 TED scrape/data", pattern = ".tar.gz",  full.names = T)

all.xml=as.data.frame(list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T))
names(all.xml)="files"
all.xml$files=as.character(all.xml$files)
all.xml$short=gsub("/|([_-]20)","",str_extract(all.xml$files, "/\\d+[_-]20"))

## loading relevant cases
load("22 TED scrape/result/TED results page - 2018-08-21.Rdata")
cases=unique(as.character(results$id))
cases=gsub("-20\\d{2}","",cases)

missing=cases[!cases %in% all.xml$short]
missing=subset(results, gsub("-20\\d{2}","",results$id) %in% missing)

missing$zip=substr(as.character(missing$publication.date),1,7)


check.zips=all.zips[unlist(str_extract_all(all.zips, "20\\d+-\\d+")) %in% unique(missing$zip)]

for(i in 10:length(check.zips)){
  zip.files=untar(check.zips[i], list=T)
  
  missing.xml=as.numeric(gsub("-20..","", subset(missing, zip==str_extract(check.zips[i],"20..-.."))$id))
  
  if(grepl(".tar.gz", zip.files)){
    untar(check.zips[i],exdir="22 TED scrape/temp/temp_zip")
    temp.zips=list.files(path = "22 TED scrape/temp/temp_zip", pattern = ".tar.gz",  full.names = T)
    
    
    for(j in 1:length(temp.zips)){
      zip.files=untar(temp.zips[j], list=T)
      extract.files=zip.files[as.numeric(gsub("/|_","",unlist(str_extract_all(zip.files,"/\\d+_")))) %in% missing.xml]
      
      if(length(extract.files)>0){
        untar(temp.zips[j],exdir="22 TED scrape/temp/completify", files=extract.files)
      }
      print(j)
      
    }
    unlink("22 TED scrape/temp/temp_zip/*.tar.gz",recursive = T)
    
  } else {
    extract.files=zip.files[as.numeric(gsub("/|_","",unlist(str_extract_all(zip.files,"/\\d+_")))) %in% missing.xml]
    
    if(length(extract.files)>0){
      untar(check.zips[i],exdir="22 TED scrape/temp/completify", files=extract.files)
    }
  }
  
  
  print(i)
} 
  
  
