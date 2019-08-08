library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
library("webdriver")
library(lubridate)
rm(list = ls())

setwd("GTA cloud")

pjs <- run_phantomjs()
remDr=Session$new(port=pjs$port)

remDr$go("https://ted.europa.eu/TED/search/expertSearch.do")

## setting to Archives
e=remDr$findElement(xpath="//input[@value='ARCHIVE']")
e$click()

### DO THIS AS AN EXPERT QUERY:
# https://ted.europa.eu/TED/misc/helpPage.do?helpPageId=expertSearch

months.back=99
end.date=as.Date(paste(year(Sys.Date()),sprintf("%02i",month(Sys.Date())),"01",sep=""), "%Y%m%d")-1
start.date=as.Date(paste(year(Sys.Date()),sprintf("%02i",month(Sys.Date())),"01",sep=""), "%Y%m%d") %m-% months(months.back)


my.query=paste("NC=[2 or 9 or Z or 3] AND PD=[",
               paste(year(start.date),sprintf("%02i",month(start.date)),sprintf("%02i",mday(start.date)),sep=""),
               " <> ",
               paste(year(end.date),sprintf("%02i",month(end.date)),mday(end.date),sep=""),
               "] AND DI=[2004/17/EC or 2014/25/EU]", sep="")

e=remDr$findElement(xpath="//textarea[@id='expertSearchCriteria.query']")
e$sendKeys(my.query)

e=remDr$findElement(xpath="//button[@title='Perform search']")
e$click()


## collecting all document numbers
html <- htmlParse(remDr$getSource()[[1]], asText=T)



results=data.frame(case.id=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlValue),
                  url=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlGetAttr, "href"),
                  description=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[3]", xmlValue),
                  country.code=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[4]", xmlValue),
                  publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"),
                  stringsAsFactors = F)

result.pages=max(as.numeric(str_extract(unique(xpathSApply(html, "//div[@class='page-icon pagelast']/descendant::a", xmlGetAttr, "href")), "\\d+$")))

ti=Sys.time()
gets=1
for(i in 2:result.pages){
  gets=gets+1
  remDr$go(paste("https://ted.europa.eu/TED/search/searchResult.do?page=",i, sep=""))
  remDr$setTimeout(pageLoad = 15000)
  html <- htmlParse(remDr$getSource()[[1]], asText=T)
  
  r=data.frame(case.id=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlValue),
               url=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlGetAttr, "href"),
               description=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[3]", xmlValue),
               country.code=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[4]", xmlValue),
               publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"),
               stringsAsFactors = F)
  
  results=unique(rbind(results, r))
  
  print(paste("Tenders found: ", nrow(results), sep=""))
  fin=ti+ as.numeric(difftime(Sys.time(),ti, unit="secs"))/i*(result.pages-i)
  print(paste("Estimated completion time: ",fin, sep=""))
  
  ## Timeout to avoid blockage:
  ## More than 500 UDL requests (HTTP GET) on the website in less than 6 minutes - access denied for 24 h (renewed on each UDL request);
  ## More than 10 000 HTTP requests in the last 15 minutes - Access denied as considered as DOS/Bad bot for 5 minutes;
  
  if(gets%%480==0){
    Sys.sleep(120)
  }
}


remDr$delete()


results$file=NA
results$gpa=NA
results$collection.date=Sys.Date()
results$file.extracted=F
results$fully.parsed=F
results$fully.hs.converted=F
results$added.to.bulk.nr=NA
results$gta.relevant=NA
results$issuer.name=NA
results$issuer.town=NA
results$issuer.country=NA
results$tender.source=NA
results$multi.doc=NA


output.path=paste("22 TED scrape/result/TED results - ",start.date," - ",end.date,".Rdata",sep="")
save(results, file=output.path)
save(results, file="22 TED scrape/result/TED results - latest search.Rdata")

# 
# 
# ## comparing to existing entries
# load("22 TED scrape/result/GTA TED replica.Rdata")
# 
# 
# ## splitting the data
# ted.cases=gtabastiat::b_bind(ted.cases, subset(results, ! case.id %in% ted.cases$case.id))
# save(ted.cases,ted.directive,ted.cpv,ted.value,ted.currencies, file="22 TED scrape/result/GTA TED replica.Rdata")
# 
# 
# ## starting bulk download
# if(nrow(subset(ted.cases, file.extracted==F))>0){
#   pub.dates=unique(subset(ted.cases, file.extracted==F)$publication.date)
#   pub.dates=unique(paste(year(pub.dates),sprintf("%02i",month(pub.dates)),sep="-"))
#   
#   ## do the login
#   
#   
#   
#   ## bulk download
#   for(pd in pub.dates){
#     y.m=unlist(strsplit(pd, "-"))
#     GET(paste("https://ted.europa.eu/xml-packages/monthly-packages/",y.m[1],"/",y.m[1],"-",y.m[2],".tar.gz",sep=""), 
#         write_disk(paste("22 TED scrape/data/",pd,".tar.gz",sep=""), overwrite=TRUE))
#   }
#   
# }
# 


