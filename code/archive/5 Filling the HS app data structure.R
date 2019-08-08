library(XML)
library(stringr)
library(splitstackshape)
library(webdriver)
library(gtalibrary)
setwd("GTA cloud")
rm(list = ls())


## some preparation before populating the tables
load("22 TED scrape/help files/cpv codes & hs chapters.Rdata")
load("22 TED scrape/help files/CPV-HS conversion processing order.Rdata")
priority.cutoffs=c(34,58,65)
priority.1=cSplit(subset(priority.processing, priority<=34), which(names(priority.processing)=="code.list"), sep=";", direction="long")$code.list
priority.2=cSplit(subset(priority.processing, priority<=58 & priority>34), which(names(priority.processing)=="code.list"), sep=";", direction="long")$code.list
priority.3=cSplit(subset(priority.processing, priority<=65 & priority>58), which(names(priority.processing)=="code.list"), sep=";", direction="long")$code.list

priority.codes=c(priority.1, priority.2, priority.3)
cpv.of.interest=subset(cpv.codes, has.children==0 & cpv %in% priority.codes)


load("22 TED scrape/help files/CPC-HS correspondence.Rdata")
cpv.checked=subset(cpv.checked, cpv.value %in% cpv.of.interest$cpv)
cpv.checked=subset(cpv.checked, as.numeric(hs.code) %in% gtalibrary::hs.names$HS12code)


## setting the priorities

cpv.checked=subset(cpv.checked, cpv.value %in%  priority.codes)


## filling the tables
## static tables
users=data.frame(user.id=1,
                 name="Johannes Fritz",
                 gta.layer="core",
                 email="johannes.fritz@unisg.ch",
                 stringsAsFactors = F)

suggestion.sources=data.frame(source.id=1:length(unique(cpv.checked$source)),
                              source.name=unique(cpv.checked$source))

levels.of.certainty=data.frame(certainty.level=1:5,
                            certainty.name=c("exactly", "highly", "fairly","somewhat","not"),
                            stringsAsFactors = F)
## dynamic tables
job.log=data.frame(job.id=1:3,
                   job.type="import",
                   job.name=paste("CPV-HS correspondence: Batch ", 1:3, sep=""),	
                   user.id=1,	
                   nr.of.checks=3,
                   check.hierarchy=F,
                   is.priority=c(F,T,T),
                   self.check=T,
                   related.state.act=NA,
                   stringsAsFactors = F)

phrase.table=data.frame(phrase.id=1:length(unique(cpv.of.interest$cpv.name)),
                        phrase=as.character(unique(cpv.of.interest$cpv.name)),
                        source="original",
                        stringsAsFactors = F)


job.phrase=data.frame(job.id=1,	
                      phrase.id=unique(phrase.table$phrase.id),
                      processed=F,
                      stringsAsFactors = F)

job.phrase$job.id[job.phrase$phrase.id %in% subset(phrase.table, phrase %in% subset(cpv.of.interest, cpv %in% priority.1)$cpv.name)$phrase.id]=3
job.phrase$job.id[job.phrase$phrase.id %in% subset(phrase.table, phrase %in% subset(cpv.of.interest, cpv %in% priority.2)$cpv.name)$phrase.id]=2

code.suggested=data.frame(suggestion.id=numeric(),
                          phrase.id=numeric(),
                          hs.code=numeric()	,
                          code.by.user=logical(),
                          stringsAsFactors = F)

for(search.term in phrase.table$phrase){
  p.id=phrase.table$phrase.id[phrase.table$phrase==search.term]
  hs.codes=unique(cpv.checked$hs.code[cpv.checked$cpv.name==search.term])
  if(length(hs.codes)==0){hs.codes=NA}
  sug.start=nrow(code.suggested)+1
  sug.end=sug.start+length(hs.codes)-1
  
  code.suggested=rbind(code.suggested,
                       data.frame(suggestion.id=sug.start:sug.end,
                            phrase.id=p.id,
                            hs.code=hs.codes,
                            code.by.user=F,
                            stringsAsFactors = F)
  )
  
}


## need to generate teh code.source here, a bit complicated (this would be from gta_hs_finder(..., aggregate=F) for you.)
find.hs=unique(cpv.checked[,c("cpv.name","hs.code", "source")] )
names(find.hs)=c("phrase", "hs.code", "source.name")

code.source=merge(code.suggested, phrase.table, by=c("phrase.id"),all.x=T)
code.source=merge(code.source, find.hs, by=c("phrase","hs.code"),all.x=T)
code.source=merge(code.source, suggestion.sources, by="source.name", all.y=T)
code.source=unique(code.source[,c("suggestion.id", "source.id")])

## tables not used (as I am not doing checks)
# check.log
# check.phrases
# words.removed
# code.selected
# check.certainty

save(job.log, levels.of.certainty, suggestion.sources, users,phrase.table, job.phrase,code.suggested,code.source, file="17 Shiny/5 HS code finder/database/GTA tariff database.Rdata")
save(job.log, levels.of.certainty, suggestion.sources, users,phrase.table, job.phrase,code.suggested,code.source, 
     file="data/tariffs/GTA tariff database.Rdata") ## Please update every time database is updated.


