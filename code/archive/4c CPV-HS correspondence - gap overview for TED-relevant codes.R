library(XML)
library(stringr)
library(splitstackshape)
library(webdriver)
setwd("GTA cloud")
rm(list = ls())
options("scipen"=999, "digits"=4)



# load("22 TED scrape/result/Final data set.Rdata")
# 
# final$id=1:nrow(final)
# 
# ted.cpv=unique(cSplit(final, which(names(final)=="cpv.value"), direction="long", sep=";")[,c("id","cpv.value")])
# 

load("22 TED scrape/result/GTA TED database.Rdata")

# FOr working with ted.gta.value 
ted.cpv=subset(ted.gta.cpv, file %in% subset(ted.gta.value, usd.value>=10000000)$file)
names(ted.cpv)=c("id", "cpv.value")
ted.cpv$cpv.value=as.numeric(as.character(ted.cpv$cpv.value))
ted.cpv=subset(ted.cpv, cpv.value<=45000000 & cpv.value>=1000000)
ted.cpv$included.codes=NA

load("22 TED scrape/help files/cpv codes & hs chapters.Rdata")


i=1
for(cd in unique(ted.cpv$cpv.value)){
  code=as.numeric(cd)
  if(cpv.codes$has.children[cpv.codes$cpv==code]==0){
    ted.cpv$included.codes[ted.cpv$cpv.value==code]=code
  } else {
    l1=cpv.codes$level1[cpv.codes$cpv==code]
    nz=cpv.codes$no.zeros[cpv.codes$cpv==code]
    
    ted.cpv$included.codes[ted.cpv$cpv.value==code]=paste(unique(subset(cpv.codes, level1==l1 & grepl(paste("^", nz, sep=""), no.zeros) & has.children==0)$cpv), collapse=",")
  }
  print(i/length(unique(ted.cpv$cpv.value)))
  i=i+1
}

ted.cpv.all=unique(cSplit(ted.cpv, which(names(ted.cpv)=="included.codes"), direction="long", sep=",")[,c("id","included.codes")])

codes.to.check=unique(ted.cpv.all$included.codes)
cases.resolved=numeric(length(codes.to.check))

all.cases=length(unique(ted.cpv.all$id))
for(i in 1:length(codes.to.check)){
  cases.resolved[i]=abs(length(unique(subset(ted.cpv.all, included.codes!=codes.to.check[i])$id))-all.cases)
  print(i/length(codes.to.check))
}

code.priority=data.frame(cpv.code=codes.to.check,
                         cases.resolved=cases.resolved)


case=unique(ted.cpv.all$id)
case.by.case=numeric(length(case))
all.cases=length(case)
for(i in 1:length(case)){
  case.codes=subset(ted.cpv.all, id==case[i])$included.codes
  case.by.case[i]=abs(length(unique(subset(ted.cpv.all,! included.codes %in% case.codes)$id))-all.cases)
  print(i/all.cases)
}

case.priority=data.frame(id=case,
                         cases.resolved=case.by.case)

case.priority=merge(case.priority, aggregate(included.codes ~id, ted.cpv.all, function(x) length(unique(x))), by="id", all.x=T)

case.priority$multiplier=case.priority$cases.resolved/case.priority$included.codes
case.priority=subset(case.priority, included.codes>1)

# What if I take the top 20 code priorities?
priority.processing=data.frame(priority=numeric(),
                               nr.of.codes.added=numeric(),
                               nr.of.cases.added=numeric(),
                               nr.of.codes.cumul=numeric(),
                               nr.of.cases.cumul=numeric(),
                               code.list=character())

p=1
priority.cpv=c()
cleared.cases=0
for(m in seq(max(c(case.priority$multiplier, code.priority$cases.resolved)),0,-.05)){
  good.singles=subset(code.priority, cases.resolved>=m)$cpv.code
  good.cases=c(subset(case.priority, multiplier>=m)$id)
  new.cpv=setdiff(c(good.singles, unique(subset(ted.cpv.all, id %in% good.cases)$included.codes)), priority.cpv)
  
  priority.cpv=c(good.singles, unique(subset(ted.cpv.all, id %in% good.cases)$included.codes))
  
  
  
  priority.processing=rbind(priority.processing, data.frame(priority=p,
                                                            nr.of.codes.added=length(new.cpv),
                                                            nr.of.codes.cumul=length(priority.cpv),
                                                            nr.of.cases.added=abs(length(unique(subset(ted.cpv.all,! included.codes %in% priority.cpv)$id))-all.cases)-cleared.cases,
                                                            nr.of.cases.cumul=abs(length(unique(subset(ted.cpv.all,! included.codes %in% priority.cpv)$id))-all.cases),
                                                            code.list=paste(new.cpv, collapse=";")))
  p=p+1
  
  cleared.cases=max(priority.processing$nr.of.cases.cumul, rm.na=T)
}
priority.processing=subset(priority.processing, nr.of.codes.added>0)
priority.processing$priority=1:nrow(priority.processing)

save(priority.processing, file="22 TED scrape/help files/CPV-HS conversion processing order.Rdata")
