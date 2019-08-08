library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
library(splitstackshape)
options("scipen"=999, "digits"=4)

rm(list = ls())

setwd("GTA cloud")

## functions
get_attr=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x))), collapse=";")
}

get_value=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x))), collapse=";")
}

get_attr_vector=function(document, attribute){
  unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x)))
}

get_value_vector=function(document, attribute){
  unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x)))
}


load("22 TED scrape/result/Relevant TED cases.Rdata")


ted.files=unique(ted.base[,c("file")])
ted.files=gsub("temp/\\d+_\\d+","relevant cases",ted.files)

ted.values=data.frame(file=character(),value.tag=character(), tag.attr=character(), tag.value=character(),
                      stringsAsFactors = F)

# for checking whether source site still exists


step=0
for(i in 1:length(ted.files)){
  doc=xmlParse(ted.files[i])
  

  print(i/length(ted.files))  
}

save(ted.values, file=paste("22 TED scrape/result/Final TED values.Rdata", sep=""))

  