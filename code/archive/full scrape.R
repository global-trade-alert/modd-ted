library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
library("webdriver")
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
e=remDr$findElement(xpath="//textarea[@id='expertSearchCriteria.query']")
e$sendKeys("NC=[2 or 9 or Z or 3] AND PD=[20180801 <> 20190228] AND DI=[2004/17/EC or 2014/25/EU]")


## collecting all document numbers
html <- htmlParse(remDr$getSource()[[1]], asText=T)


load("22 TED scrape/result/TED results page - 2018-08-15.Rdata")
# results=data.frame(id=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlValue),
#                    url=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlGetAttr, "href"),
#                    description=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[3]", xmlValue),
#                    country.code=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[4]", xmlValue),
#                    publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"))
# 

result.pages=max(as.numeric(str_extract(unique(xpathSApply(html, "//div[@class='page-icon pagelast']/descendant::a", xmlGetAttr, "href")), "\\d+$")))

ti=Sys.time()
for(i in 2:result.pages){
  remDr$go(paste("https://ted.europa.eu/TED/search/searchResult.do?page=",i, sep=""))
  remDr$setTimeout(pageLoad = 15000)
  html <- htmlParse(remDr$getSource()[[1]], asText=T)
  
  r=data.frame(id=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlValue),
               url=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[2]/descendant::a", xmlGetAttr, "href"),
               description=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[3]", xmlValue),
               country.code=xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[4]", xmlValue),
               publication.date=as.Date(xpathSApply(html, "//div[@class='aroundTable']/descendant::tbody/tr/td[5]", xmlValue), "%d/%m/%Y"))
  
  results=unique(rbind(results, r))
  
  print(paste("Tenders found: ", nrow(results), sep=""))
  fin=ti+ as.numeric(difftime(Sys.time(),ti, unit="secs"))/i*(result.pages-i)
  print(paste("Estimated completion time: ",fin, sep=""))
}


remDr$delete()

save(results, file=paste("22 TED scrape/result/TED results page - ",Sys.Date(),".Rdata", sep=""))

results$value.description=NA
results$cpv.main=NA
results$cpv.additional=NA
results$tenders.total=NA
results$tenders.eu=NA
results$tenders.noneu=NA
results$gpa=NA
results$performance.place.name=NA
results$performance.place.code=NA
results$recipient=NA
results$recipient.nuts.code=NA
results$recipient.nuts.name=NA
results$recipient.sme=NA

## now extracting the values
ti=Sys.time()
for(i in 1:nrow(results)){
  page=read_html(paste("https://ted.europa.eu/udl?uri=TED:NOTICE:",results$id[i],":TEXT:EN:HTML&src=0", sep=""))
  html=htmlParse(page, asText=T)
  
  ## value
  if(length(xpathSApply(html, "//span[contains(text(), 'II.1.7)') and not(contains(text(),'III'))]/following-sibling::div", xmlValue))>0){
    results$cpv.codes[i]=xpathSApply(html, "//span[contains(text(), 'II.1.7)') and not(contains(text(),'III'))]/following-sibling::div", xmlValue)
  }
  
  ## CPV codes main
  if(length(xpathSApply(html, "//span[contains(text(), 'II.1.2)') and not(contains(text(),'III'))]/following-sibling::div/span", xmlValue))>0){
    results$cpv.main[i]=paste(paste(xpathSApply(html, "//span[contains(text(), 'II.1.2)') and not(contains(text(),'III'))]/following-sibling::div/span", xmlValue),
                                    xpathSApply(html, "//span[contains(text(), 'II.1.2)') and not(contains(text(),'III'))]/following-sibling::div/span", xmlGetAttr, "title"),
                                    sep=" - "),
                              collapse=";")
  }
  
  ## CPV codes additional
  if(length(xpathSApply(html, "//span[contains(text(), 'II.2.2)') and not(contains(text(),'III'))]/following-sibling::div/span", xmlValue))>0){
    results$cpv.additional[i]=paste(paste(xpathSApply(html, "//span[contains(text(), 'II.2.2)') and not(contains(text(),'III'))]/following-sibling::div/span", xmlValue),
                                    xpathSApply(html, "//span[contains(text(), 'II.2.2)') and not(contains(text(),'III'))]/following-sibling::div/span", xmlGetAttr, "title"),
                                    sep=" - "),
                              collapse=";")
  }
  
  
  ## Tender info
  if(length(xpathSApply(html, "//span[contains(text(), 'V.2.2)') and not(contains(text(),'IV'))]/following-sibling::div[contains(text(),'Number of tenders received:')]", xmlValue))>0){
    results$tenders.total[i]=as.numeric(str_extract(xpathSApply(html, "//span[contains(text(), 'V.2.2)') and not(contains(text(),'IV'))]/following-sibling::div[contains(text(),'Number of tenders received:')]", xmlValue), "\\d+$"))
    
    if(length(xpathSApply(html, "//span[contains(text(), 'V.2.2)') and not(contains(text(),'IV'))]/following-sibling::div[contains(text(),'Number of tenders received from tenderers from other EU Member States')]", xmlValue))>0){
        results$tenders.eu[i]=as.numeric(str_extract(xpathSApply(html, "//span[contains(text(), 'V.2.2)') and not(contains(text(),'IV'))]/following-sibling::div[contains(text(),'Number of tenders received from tenderers from other EU Member States')]", xmlValue), "\\d+$"))
    }
    
    if(length(xpathSApply(html, "//span[contains(text(), 'V.2.2)') and not(contains(text(),'IV'))]/following-sibling::div[contains(text(),'Number of tenders received from tenderers from non-EU Member States')]", xmlValue))>0){
        results$tenders.noneu[i]=as.numeric(str_extract(xpathSApply(html, "//span[contains(text(), 'V.2.2)') and not(contains(text(),'IV'))]/following-sibling::div[contains(text(),'Number of tenders received from tenderers from non-EU Member States')]", xmlValue), "\\d+$"))
    }
  }
  
  
  # GPA incl?
  if(length(xpathSApply(html, "//span[contains(text(), 'IV.1.8)')]/following-sibling::div", xmlValue))>0){
    results$gpa[i]=as.numeric(grepl("yes$",xpathSApply(html, "//span[contains(text(), 'IV.1.8)')]/following-sibling::div", xmlValue)))
  }
 
  # performance place
  if(length(xpathSApply(html, "//span[contains(text(), 'II.2.3)')and not(contains(text(),'III'))]/following-sibling::div/span[@class='nutsCode']", xmlValue))>0){
    results$performance.place.name[i]=xpathSApply(html, "//span[contains(text(), 'II.2.3)')and not(contains(text(),'III'))]/following-sibling::div/span[@class='nutsCode']", xmlValue)
    results$performance.place.code[i]=xpathSApply(html, "//span[contains(text(), 'II.2.3)')and not(contains(text(),'III'))]/following-sibling::div/span[@class='nutsCode']", xmlGetAttr, "title")
  }
  
  
   
  # recipient info
  if(length(xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div[1]", xmlValue))>0){
    results$recipient[i]=xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div[1]/descendant::text()[1]", xmlValue)
    if(length(xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div/span[@class='nutsCode']", xmlValue))>0){
      results$recipient.nuts.code[i]=xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div/span[@class='nutsCode']", xmlValue)
      results$recipient.nuts.name[i]=xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div/span[@class='nutsCode']", xmlGetAttr, "title")
    }
    if(length(xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div[2]", xmlValue))>0){
      results$recipient.sme[i]=as.numeric(grepl("yes$",xpathSApply(html, "//span[contains(text(), 'V.2.3)')and not(contains(text(),'IV'))]/following-sibling::div[2]", xmlValue)))
    }
  }
  
  
  fin=ti+ as.numeric(difftime(Sys.time(),ti, unit="secs"))/i*(nrow(results)-i)
  print(paste("Estimated completion time: ",fin, sep=""))
}


save(results, file=paste("22 TED scrape/result/TED scrape - ",Sys.Date(),".Rdata", sep=""))
