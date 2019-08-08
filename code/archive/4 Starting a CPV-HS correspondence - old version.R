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


hs.page="https://eurostat.prod.3ceonline.com"
cpv$hs.code=NA

pjs <- run_phantomjs()
remDr=Session$new(port=pjs$port)

remDr$go(hs.page)

last=1
while(last<=nrow(cpv)){
for(i in last:nrow(cpv)){
  remDr$go(hs.page)
  e=remDr$findElement(xpath="//textarea[@id='ccce-queryBox']")
  e$sendKeys(as.character(cpv$cpv.name[i]))
  e$sendKeys(key$enter)
  Sys.sleep(2.5)
  html <- htmlParse(remDr$getSource()[[1]], asText=T)
  if(length(xpathSApply(html, "//div[@id='hscode']",xmlValue))>0){
    cpv$hs.code[i]=as.character(paste(unlist(str_extract_all(xpathSApply(html, "//div[@id='hscode']",xmlValue), "\\d+")),collapse=""))
  }
  
  print(i)
  print(cpv$hs.code[i])
}
last=i
save(cpv, file="22 TED scrape/help files/CPC-HS correspondence.Rdata")
}

save(cpv, file="22 TED scrape/help files/CPC-HS correspondence.Rdata")
