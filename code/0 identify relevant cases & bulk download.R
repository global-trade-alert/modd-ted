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

months.back=6
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

Sys.sleep(2)
## collecting all document numbers
html <- htmlParse(remDr$getSource()[[1]], asText=T)



# results=data.frame(case.id=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlValue),
#                   url=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlGetAttr, "href"),
#                   description=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[3]", xmlValue),
#                   country.code=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[4]", xmlValue),
#                   publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"),
#                   stringsAsFactors = F)

results=data.frame(case.id=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlValue),
                   publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"),
                   collection.date=Sys.Date(),
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
               publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"),
               collection.date=Sys.Date(),
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

load("8 Data dumps/22 TED scrape/log/TED log.Rdata")

ted.log=gtabastiat::bt_bind(ted.log, subset(results, ! case.id %in% ted.log$case.id))
ted.log[is.na(ted.log)]=F

save(ted.log, "8 Data dumps/22 TED scrape/log/TED log.Rdata")