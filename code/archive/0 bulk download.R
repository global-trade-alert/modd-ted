library("httr")
setwd("GTA cloud")


load("22 TED scrape/result/TED processed cases.Rdata")

if(nrow(subset(ted.processed, fully.processed==F))>0){
  pub.dates=unique(subset(ted.processed, fully.processed==F)$publication.date)
  pub.dates=unique(paste(year(pub.dates),sprintf("%02i",month(pub.dates)),sep="-"))
  
  
  ## do the login
  
  ## bulk download
  for(pd in pub.dates){
    y.m=unlist(strsplit(pd, "-"))
    GET(paste("https://ted.europa.eu/xml-packages/monthly-packages/",y.m[1],"/",y.m[1],"-",y.m[2],".tar.gz",sep=""), 
        write_disk(paste("22 TED scrape/data/",y.m[1],"-",y.m[2],".tar.gz",sep=""), overwrite=TRUE))
  }
  
}

