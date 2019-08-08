library(xlsx)
library(data.table)
library(splitstackshape)
setwd("GTA cloud")

load("22 TED scrape/result/TED xml.Rdata")

results=subset(results, is.na(value.number)==F)
results$publication.date=as.Date(results$publication.date, origin="1970-01-01")
unique(results$value.currency)
results$value.number=as.numeric(as.character(results$value.number))

currencies=data.frame(value.currency=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK"),
                      divide.by=c(.8, 3.7, .85, 8.2, 6, 260, 1.6, 22, 8, 1,6,1,3.8, 26, 51, 105))

results=merge(results, currencies, by="value.currency", all.x=T)
results$value.usd=results$value.number/results$divide.by

results$domestic.contractor=as.numeric(substr(results$issuer.nuts,1,2)==substr(results$recipient.nuts.code,1,2))

summary(results$value.usd, detail)
results$t.id=1:nrow(results)

tender.cpv=results[,c("t.id", "cpv.main")]
tender.cpv$type="main"
t.cpv=results[,c("t.id", "cpv.additional")]
t.cpv$type="additional"
names(tender.cpv)=c("t.id", "cpv","type")
names(t.cpv)=c("t.id", "cpv","type")

tender.cpv=rbind(cSplit(tender.cpv, 2, direction = "long", sep=";"),cSplit(t.cpv, 2, direction = "long", sep=";"))
length(unique(tender.cpv$t.id))
tender.cpv$cpv.category=substr(tender.cpv$cpv,1,5)

cpv.names=read.csv("22 TED scrape/help files/cpv_names.csv", sep=";")
cpv.categories=cpv.names[grepl("000$",cpv.names$cpv),]
cpv.categories$cpv[nchar(cpv.categories$cpv)==7]=paste("0",cpv.categories$cpv[nchar(cpv.categories$cpv)==7], sep="")
cpv.categories$cpv.category=substr(cpv.categories$cpv,1,5)
setnames(cpv.categories, "cpv.name", "category.name")
cpv.categories=unique(cpv.categories[c("cpv.category", "category.name")])


tender.cpv=merge(tender.cpv, cpv.categories, by="cpv.category", all.x=T)

## stats
no.award=subset(results, is.na(recipient.nuts.code))
results=subset(results, is.na(recipient.nuts.code)==F)

## overview
overview=data.frame(year=c(2017,2018), 
                           entries.total=c(nrow(subset(results, year(publication.date)==2017)),nrow(subset(results, year(publication.date)==2018))),
                           entries.usd10=c(nrow(subset(results, year(publication.date)==2017 & value.usd>=10000000)),nrow(subset(results, year(publication.date)==2018  & value.usd>=10000000))),
                           entries.gpa=c(nrow(subset(results, year(publication.date)==2017 & gpa==0)),nrow(subset(results, year(publication.date)==2018 & gpa==0))),
                           entries.gpa.usd10=c(nrow(subset(results, year(publication.date)==2017 & value.usd>=10000000 & gpa==0)),nrow(subset(results, year(publication.date)==2018  & value.usd>=10000000 & gpa==0))),
                           entries.gpa.usd10.same=c(nrow(subset(results, year(publication.date)==2017 & value.usd>=10000000 & gpa==0 & domestic.contractor==1)),nrow(subset(results, year(publication.date)==2018  & value.usd>=10000000 & gpa==0 & domestic.contractor==1))))

names(overview)=c("Year", "Number of tenders", "Tenders above USD 10 million", "Tenders not subject to GPA", "Tenders above USD 10mn and not subject to GPA", "Tenders >USD 10mn, GPA=0 and contract=domestic")
write.xlsx(overview, file="22 TED scrape/result/TED Utilites 2017-today - non-GPA.xlsx", sheetName = "Tenders overview", row.names = F)

## by country
restricted.set=subset(results, value.usd>=10000000 & gpa==0 & domestic.contractor==1)
restricted.set$publication.year=year(restricted.set$publication.date)

tender.view=aggregate(t.id ~ issuer.country + publication.year, restricted.set, function(x) length(unique(x)))
tender.view=merge(tender.view,
                  aggregate(value.usd ~ issuer.country + publication.year, restricted.set, sum), 
                  by=c("issuer.country","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "sum.value")
tender.view=merge(tender.view,
                  aggregate(value.usd ~ issuer.country + publication.year, restricted.set, mean), 
                  by=c("issuer.country","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "mean.value")
tender.view=merge(tender.view,
                  aggregate(value.usd ~ issuer.country + publication.year, restricted.set, median), 
                  by=c("issuer.country","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "median.value")
tender.view=merge(tender.view,
                  aggregate(value.usd ~ issuer.country + publication.year, restricted.set, max), 
                  by=c("issuer.country","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "max.value")

names(tender.view)=c("Awarding country","Year", "Number of tenders", "Total value", "Average value", "Median value", "Maximum value")
write.xlsx(tender.view, file="22 TED scrape/result/TED Utilites 2017-today - non-GPA.xlsx", sheetName = "tenders by country", row.names = F, append=T)


## CPV categories
mains=aggregate(cpv.category  ~t.id, tender.cpv, function(x) length(unique(x)))
names(mains)=c("t.id", "nr.categories")

tender.cpv=merge(tender.cpv, mains, by="t.id", all.x=T)  

restricted.set=merge(restricted.set, subset(tender.cpv, type=="main")[,c("t.id", "cpv.category", "category.name", "nr.categories")], 
                     by=c("t.id"), all.x=T)
restricted.set$value.usd=restricted.set$value.usd/restricted.set$nr.categories

tender.view=aggregate(t.id ~ cpv.category + category.name + publication.year, restricted.set, function(x) length(unique(x)))
tender.view=merge(tender.view,
                  aggregate(value.usd ~ cpv.category + category.name + publication.year, restricted.set, sum), 
                  by=c("cpv.category", "category.name","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "sum.value")
tender.view=merge(tender.view,
                  aggregate(value.usd ~ cpv.category + category.name + publication.year, restricted.set, mean), 
                  by=c("cpv.category", "category.name","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "mean.value")
tender.view=merge(tender.view,
                  aggregate(value.usd ~ cpv.category + category.name + publication.year, restricted.set, median), 
                  by=c("cpv.category", "category.name","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "median.value")
tender.view=merge(tender.view,
                  aggregate(value.usd ~ cpv.category + category.name + publication.year, restricted.set, max), 
                  by=c("cpv.category", "category.name","publication.year"), all.x=T)
setnames(tender.view, "value.usd", "max.value")

names(tender.view)=c("CPV category","CPV name","Year", "Number of tenders", "Total value", "Average value", "Median value", "Maximum value")
write.xlsx(tender.view, file="22 TED scrape/result/TED Utilites 2017-today - non-GPA.xlsx", sheetName = "tenders by CPV category", row.names = F, append=T)


write.xlsx(restricted.set, file="22 TED scrape/result/TED Utilites 2017-today - non-GPA.xlsx", sheetName = "raw tender data", row.names = F, append=T)
