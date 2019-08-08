library(XML)
library(stringr)
library(splitstackshape)
library(webdriver)
setwd("GTA cloud")
rm(list = ls())

cpv.codes=read.csv("22 TED scrape/help files/cpv_names.csv",sep=";")

cpv.codes$this.level=7-nchar(str_extract(cpv.codes$cpv, "0*$"))
cpv.codes$no.zeros=gsub("0*$","", cpv.codes$cpv)
cpv.codes$level1=substr(cpv.codes$cpv,1,2)
cpv.codes$level1[nchar(cpv.codes$cpv)==7]=substr(cpv.codes$cpv[nchar(cpv.codes$cpv)==7],1,1)

cpv.codes$has.children=apply(cpv.codes, 1, function(x) as.numeric(nrow(subset(cpv.codes, grepl(paste("^",x[which(names(cpv.codes)=="no.zeros")],sep=""),no.zeros) & 
                                                                                this.level>x[which(names(cpv.codes)=="this.level")] &
                                                                                level1==x[which(names(cpv.codes)=="level1")]))>0))

most.granular=subset(cpv.codes, has.children==0 & cpv<=45000000)

## adding HS code chapter(s)

hs.cpv=read.csv("22 TED scrape/help files/hs-cpv.csv", sep=";")
setnames(hs.cpv, names(hs.cpv)[1], "hs.chapter")
cpv.codes=merge(cpv.codes, aggregate(hs.chapter ~ cpv, hs.cpv, function(x) paste(unique(x), collapse=",")), by="cpv", all.x=T)

cpv.codes$parent=NA
cpv.codes$parent[nchar(cpv.codes$no.zeros)>1]=substr(cpv.codes$no.zeros[nchar(cpv.codes$no.zeros)>1],1,(nchar(cpv.codes$no.zeros)-1))

save(cpv.codes, file="22 TED scrape/help files/cpv codes & hs chapters.Rdata")

