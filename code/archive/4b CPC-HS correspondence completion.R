library(XML)
library(stringr)
library(splitstackshape)
library(webdriver)
setwd("GTA cloud")
rm(list = ls())

load("22 TED scrape/help files/CPC-HS correspondence.Rdata")
load("22 TED scrape/result/Final data set - 2018-09-07.Rdata")


ted.cpv=unique(cSplit(final, which(names(final)=="cpv.value"), direction="long", sep=";")$cpv.value)


needed.for.ted=subset(cpv.to.hs, cpv.to.hs$cpv %in% ted.cpv)
needed.for.ted=subset(needed.for.ted, cpv<45000000)

needed.for.ted=subset(needed.for.ted, is.na(hs.code)==T)


load("22 TED scrape/help files/cpv codes & hs chapters.Rdata")
cpv.codes$no.zeros[nchar(cpv.codes$cpv)==7]=paste("0",cpv.codes$no.zeros[nchar(cpv.codes$cpv)==7], sep="")

no.zeros=subset(cpv.codes, code.full %in% needed.for.ted$code.full)$no.zeros

cpv.codes$needs.check=0

for(i in 1:length(no.zeros)){
  cpv.codes$needs.check[grepl(paste("^", no.zeros[i], sep=""),cpv.codes$no.zeros)]=1  
  
  print(i)
}

## needed for ted incl. subchapters: takes the last node for each CPV branch.
nft.incl.subchapters=subset(cpv.codes, has.children==0 & needs.check==1)
nft.incl.subchapters$cpv.name=tolower(nft.incl.subchapters$cpv.name)


pjs <- run_phantomjs()
remDr=Session$new(port=pjs$port)
hs.page="https://eurostat.prod.3ceonline.com"

nft.incl.subchapters$potential.hs=NA
nft.incl.subchapters$source=NA

for(i in 1:nrow(nft.incl.subchapters)){
  
  remDr$go(hs.page)
  html <- htmlParse(remDr$getSource()[[1]], asText=T)
  if(length(xpathSApply(html,"//textarea[@id='ccce-queryBox']", xmlValue))>0){
    
    e=remDr$findElement(xpath="//textarea[@id='ccce-queryBox']")
    e$sendKeys(as.character(nft.incl.subchapters$cpv.name[i]))
    e$sendKeys(key$enter)
    Sys.sleep(2.5)
    
    html <- htmlParse(remDr$getSource()[[1]], asText=T)
    if(length(xpathSApply(html, "//div[@id='hscode']",xmlValue))>0){
      nft.incl.subchapters$potential.hs[i]=as.character(paste(unlist(str_extract_all(xpathSApply(html, "//div[@id='hscode']",xmlValue), "\\d+")),collapse=""))
      nft.incl.subchapters$source[i]="matched via Eurostat HS code finder ('https://eurostat.prod.3ceonline.com')"
      print(nft.incl.subchapters$potential.hs[i])
    }else {
      
      remDr$go(paste("https://www.tariffnumber.com/2013/", gsub(" ","%20", nft.incl.subchapters$cpv.name[i]),sep=""))
      
      html <- htmlParse(remDr$getSource()[[1]], asText=T)
      
      if(length(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6])>0){
        nft.incl.subchapters$potential.hs[i]=paste(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6], collapse=",")
        nft.incl.subchapters$source[i]="matched via European customs portal ('https://www.tariffnumber.com', 2013 edition)"
        print(nft.incl.subchapters$potential.hs[i])
      }
      
    }
    
  }else {
    
    remDr$go(paste("https://www.tariffnumber.com/2013/", gsub(" ","%20", nft.incl.subchapters$cpv.name[i]),sep=""))
    
    html <- htmlParse(remDr$getSource()[[1]], asText=T)
    
    if(length(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6])>0){
      nft.incl.subchapters$potential.hs[i]=paste(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6], collapse=",")
      nft.incl.subchapters$source[i]="matched via European customs portal ('https://www.tariffnumber.com', 2013 edition)"
      print(nft.incl.subchapters$potential.hs[i])
    }
    
  }

  
  
  print(i/nrow(nft.incl.subchapters))
  
}
remDr$delete()

## CHECK MOST GRANULAR NOW:

accepted=subset(nft.incl.subchapters, is.na(potential.hs)==F & grepl(",", potential.hs)==F)
accepted$hs.code=accepted$potential.hs
accepted$potential.hs=NULL
setnames(accepted, "source", "hs.source")
accepted$no.zeros=gsub("^0","",accepted$no.zeros)
accepted$parent=substr(accepted$no.zeros,1,nchar(accepted$no.zeros)-1)

cpv.to.hs=rbind(subset(cpv.to.hs, !cpv %in% accepted$cpv), accepted[,c(names(cpv.to.hs))])
cpv.to.hs$source[is.na(cpv.to.hs$hs.code)]=NA


start=nrow(subset(cpv.to.hs, is.na(hs.code)==F))
finish=nrow(subset(cpv.to.hs, is.na(hs.code)==F))+1
while(start<finish){
  complete.parents=cpv.to.hs
  complete.parents$has.hs=as.numeric(is.na(complete.parents$hs.code)==F)
  complete.parents=subset(complete.parents, parent %in% subset(aggregate(has.hs ~ parent, complete.parents, mean), has.hs==1)$parent)
  complete.parents=aggregate(hs.code ~ parent, complete.parents, function(x) toString(unique(x)))
  
  cpv.p=subset(cpv.to.hs, no.zeros %in% complete.parents$parent)
  cpv.p$hs.source="based on the HS codes of its children"
  cpv.p$hs.code=apply(cpv.p, 1, function(x) complete.parents$hs.code[complete.parents$parent==x[which(names(cpv.p)=="no.zeros")]])
  
  cpv.to.hs=rbind(subset(cpv.to.hs, ! no.zeros %in% complete.parents$parent), cpv.p)
  finish=nrow(subset(cpv.to.hs, is.na(hs.code)==F))
  print(finish)
}

save(cpv.to.hs, file="22 TED scrape/help files/CPC-HS correspondence.Rdata")


needed.for.ted=subset(nft.incl.subchapters, ! code.full %in% accepted$code.full )
check=subset(needed.for.ted, is.na(potential.hs)==F)
no.idea=subset(needed.for.ted, is.na(potential.hs)==T)
save(check, no.idea, file="22 TED scrape/help files/CPC-HS correspondence - remainder.Rdata")
