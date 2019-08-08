library(XML)
library(stringr)
library(splitstackshape)
library(webdriver)
setwd("GTA cloud")
rm(list = ls())


# for all found codes have to be within the hs chapters associated to that order

load("22 TED scrape/help files/cpv codes & hs chapters.Rdata")
cpv.of.interest=subset(cpv.codes, has.children==0)

step=1
while(step<=max(cpv.of.interest$this.level) & nrow(subset(cpv.of.interest, is.na(hs.chapter)))>0){
  
  for(i in which(is.na(cpv.of.interest$hs.chapter) & cpv.of.interest$this.level>step)){
    
    parent=substr(cpv.of.interest$no.zeros[i],1,nchar(cpv.of.interest$no.zeros[i])-step)
    if(nrow(subset(cpv.codes, no.zeros==parent &is.na(hs.chapter)==F))>0){
      cpv.of.interest$hs.chapter[i]=paste(cpv.codes$hs.chapter[cpv.codes$no.zeros==parent] ,collapse=",")
    }
  
  }
  print(step)
  step=step+1
}
cpv.of.interest$hs.chapter=gsub("NA,?","",cpv.of.interest$hs.chapter)

hs.chapter.restriction=cSplit(unique(cpv.of.interest[,c("cpv","cpv.name","hs.chapter")]), 3, direction="long", sep=",")
hs.chapter.restriction=subset(hs.chapter.restriction, is.na(hs.chapter)==F)
save(hs.chapter.restriction, file="22 TED scrape/help files/CPC-HS correspondence - hs restriction.Rdata")



