library(XML)
library(stringr)
library(splitstackshape)
library(IMFData)
setwd("GTA cloud")
rm(list = ls())
options("scipen"=999, "digits"=4)

load("22 TED scrape/result/relevance.Rdata")

ted.base=subset(relevance.all, is.na(value.value)==F)
rm(relevance.all)

# removing GPA cases
ted.base=subset(ted.base, grepl("YES", gpa, ignore.case = T)==F)
ted.base$id=1:nrow(ted.base)


## Extracting CPV values
ted.cpv=unique(ted.base[,c("id", "cpv.tag", "cpv.value")])
ted.cpv=cSplit(ted.cpv, which(names(ted.cpv)=="cpv.value"), direction="long", sep=";")
cpv.names=read.csv("22 TED scrape/help files/cpv_names.csv", sep=";")
cpv.names$type="good"
cpv.names$type[cpv.names$cpv>=45000000]="service"
cpv.names=subset(cpv.names, cpv %in% ted.cpv$cpv.value)

ted.base=subset(ted.base, id %in% subset(ted.cpv, cpv.value %in% subset(cpv.names, type=="good")$cpv)$id)

## extracting values
ted.value=unique(ted.base[,c("id","date", "value.tag", "value.cur","value.value")])
ted.value=cSplit(ted.value, which(names(ted.value)=="value.value"), direction="long", sep=";")
ted.value=cSplit(ted.value, which(names(ted.value)=="value.value"), direction="long", sep=",")

non.split=subset(ted.value, is.na(str_extract(value.value, " \\d{4,} "))==F)
non.split$odd.values=str_extract(non.split$value.value, " \\d{4,} ")
non.split=subset(non.split, is.na(odd.values)==F)
non.split$odd.values=gsub("(^\\d{3})(\\d+)","\\1;\\2", gsub(" ","",non.split$odd.values))
non.split$value.value=apply(non.split, 1, function(x) gsub(" \\d{4,} ",x[which(names(non.split)=="odd.values")],x[which(names(non.split)=="value.value")]))
non.split$odd.values=NULL
non.split=cSplit(non.split, which(names(non.split)=="value.value"), direction="long", sep=";")
ted.value=rbind(subset(ted.value, !id %in% non.split$id),non.split)

ted.value$value.value=as.numeric(gsub(" ","",ted.value$value.value))

ted.currencies=unique(ted.value[,c("id", "date","value.cur")])
ted.currencies$value.cur=apply(ted.currencies, 1, function(x) paste(unlist(str_extract_all(x[which(names(ted.currencies)=="value.cur")], "[A-Z]+")), collapse=";"))

ted.currencies=aggregate(value.cur ~id + date, cSplit(ted.currencies, which(names(ted.currencies)=="value.cur"), direction="long", sep=";"), function(x) paste(unique(x), collapse=";"))
ted.currencies=cSplit(ted.currencies, which(names(ted.currencies)=="value.cur"), direction="long", sep=";")

# 
# # convert currencies a bit ad hoc# via https://www.xe.com/currencycharts/?from=USD&to=MTL&view=5Y
# currencies=data.frame(value.cur=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK", "JPY", "LTL", "LVL", "MTL"),
#                       divide.by=c(.8, 3.7, .85, 8.2, 6, 260, 1.6, 22, 8, 1,6,1,3.8, 26, 51, 105, 100, 2.85, .6, .355))
# ted.currencies=merge(ted.currencies, currencies, by="value.cur", all.x=T)
## there are some with different currencies inside the same tender. I assume that the stronger currency is the true one to avoid deleting a worthy entry.



# IFS
# IFS.available.codes <- DataStructureMethod('IFS') # Get dimension code of IFS dataset
# IFS.search=as.data.frame(IFS.available.codes[[2]]) # Possible code in the country dimension

imf.cur=data.frame(value.cur=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK", "JPY", "LTL", "LVL", "MTL"),
                   imf.symbol=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"))

ted.currencies=merge(ted.currencies, imf.cur, by="value.cur", all.x=T)
ted.currencies$imf.symbol=as.character(ted.currencies$imf.symbol)

ted.currencies$date=as.Date(ted.currencies$date, "%Y%m%d")
ted.currencies$y.m=paste(year(ted.currencies$date), month(ted.currencies$date), sep="-")
ted.currencies$y.m[month(ted.currencies$date)<10]=paste(year(ted.currencies$date[month(ted.currencies$date)<10]),"-0", month(ted.currencies$date[month(ted.currencies$date)<10]), sep="")




databaseID <- 'IFS'
startdate='2008-11-01'
enddate=Sys.Date()
checkquery = FALSE


queryfilter <- list(CL_FREQ='M', CL_AREA_IFS=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"), CL_INDICATOR_IFS =c('ENDA_XDC_USD_RATE'))
ex.rate <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy = T)

ex.rate=ex.rate[,c(1,2,4)]
names(ex.rate)=c("y.m","divide.by", "imf.symbol")


ted.currencies=merge(ted.currencies, ex.rate, by=c("y.m", "imf.symbol"), all.x=T)

ted.currencies$divide.by[ted.currencies$id==1216]=22.8

ted.value=merge(ted.value, aggregate(divide.by ~id, ted.currencies, min), all.x=T)

ted.value$divide.by=as.numeric(ted.value$divide.by)
ted.value$value.usd=ted.value$value.value/ted.value$divide.by

ted.base=subset(ted.base, id %in% subset(ted.value, value.usd>10000000)$id)

summary(ted.value$value.usd[ted.value$value.usd>10000000])

## directive
ted.directive=unique(ted.base[,c("id","directive")])
ted.directive=cSplit(ted.directive, 2, direction="long", sep=";")

ted.base=subset(ted.base, id %in% subset(ted.directive, directive %in% c("2004/17/EC","2014/25/EU"))$id)

## copying them into a single folder.
file.copy(ted.base$file, "22 TED scrape/relevant cases", overwrite = T)
save(ted.base, ted.currencies, ted.cpv, ted.value, ted.directive, file="22 TED scrape/result/Relevant TED cases")
