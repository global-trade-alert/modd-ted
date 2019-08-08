library(stringr)
rm(list = ls())
setwd("GTA cloud")

load("22 TED scrape/result/TED results - latest search.Rdata")

## Extracting zips
if(nrow(subset(results, file.extracted==F))>0){
  pub.dates=unique(subset(results, file.extracted==F)$publication.date)
  pub.dates=unique(paste(year(pub.dates),sprintf("%02i",month(pub.dates)),sep="-"))
  
  ## restricting the zips
  all.zips=list.files(path = "22 TED scrape/data", pattern = ".tar.gz",  full.names = T)
  all.zips=all.zips[grepl(paste(pub.dates, collapse="|"), all.zips)]
  
  relevant.files=gsub("-","_",subset(results, file.extracted==F)$case.id)
  
  ## cleaning out the temp folder from XML files
  old.files=list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
  unlink(old.files)
  rm(old.files)
  
  ## extracting the relevant XML files
  for(zip in 1:length(all.zips)){
    untar(all.zips[zip],exdir="22 TED scrape/temp")
    
    sub.tar=list.files("22 TED scrape/temp", pattern = ".tar.gz", full.names = TRUE)
    if(length(sub.tar)>0){
      for(i in 1:length(sub.tar)){
        untar(sub.tar[i],exdir="22 TED scrape/temp")
        print(sub.tar[i])
      }
    }
    print(zip)
    unlink("22 TED scrape/temp/*.tar.gz",recursive = T)
    all.files=list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
    
    remove.files=all.files[!gsub("^0+","",str_extract(str_extract(all.files, "\\d+_\\d+.xml"), "\\d+_\\d+")) %in% relevant.files]
    unlink(remove.files)
    rm(all.files)
  }
  
  all.files=list.files(path = "22 TED scrape/temp", pattern = ".xml",  full.names = T, recursive = T)
  relevant.files=all.files[gsub("^0+","",str_extract(str_extract(all.files, "\\d+_\\d+.xml"), "\\d+_\\d+")) %in% relevant.files]
  
  new.location=paste("22 TED scrape/relevant cases/",unlist(str_extract_all(all.files, "\\d+_\\d+.xml$")),sep="")
  
  # copying them over
  for(i in 1:length(new.location)){
    file.copy(from=relevant.files[i], to=new.location[i], overwrite = T)
    
    
    print(i/length(new.location))
  }
  
  for(i in 1:nrow(results)){
    if(length(new.location[grepl(gsub("-","_",results$case.id[i]), new.location)])>0){
      results$file[i]=paste(new.location[grepl(gsub("-","_",results$case.id[i]), new.location)], collapse=";")
      results$file.extracted[i]=T
    }
    print(i/nrow(results))
  }

  save(results, file="22 TED scrape/result/TED results - latest search.Rdata")
  
  # cleaning up
  do.call(file.remove, list(list.files("22 TED scrape/temp", full.names = TRUE)))
  
}


