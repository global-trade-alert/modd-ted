library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
library(splitstackshape)
options("scipen"=999, "digits"=4)

rm(list = ls())

setwd("GTA cloud")
load("22 TED scrape/result/Final TED values.Rdata")

## cleaning values from odd cases, pasted entries.
DO THAT

## convert to numeric
ted.values$value.num=as.numeric(gsub("\\s","", gsub(",","\\.",ted.values$tag.value)))
ted.values=subset(ted.values, value.num>=30) ## removing VAT rates

## using the relevant number
DO THAT

## adding USD value
ted.values=merge(ted.values, final[,c("file", "date")], by="file", all.x=T)
ted.values$y.m=gsub("-(\\d{1})$","-0\\1",paste(year(ted.values$date), month(ted.values$date), sep="-"))

currency.symbol=aggregate(tag.attr ~file, cSplit(ted.values, which(names(ted.values)=="tag.attr"), sep=";", direction="long"), function(x) paste(unique(x), collapse=";"))
currency.symbol$multi.cur=as.numeric(grepl(";", currency.symbol$tag.attr))
names(currency.symbol)=c("file","value.cur","multi.cur")
ted.values=merge(ted.values, currency.symbol, by="file", all.x=T)

ted.currencies$id=NULL
ted.currencies$date=NULL
ted.currencies=unique(ted.currencies)
ted.currencies$divide.by=as.numeric(ted.currencies$divide.by)

ted.values=merge(ted.values, ted.currencies[,c("y.m", "value.cur","divide.by")], by=c("y.m","value.cur"), all.x=T)

ted.values$value.usd=ted.values$value.num/ted.values$divide.by

# 
# test=aggregate(value.usd ~file, ted.values, min)
# 
# contract.value=as.data.frame(table(ted.values$file))
# names(contract.value)=c("file", "number.of.values")
# contract.value=merge(contract.value, aggregate(), by="file", all.x=T)
# ted.value=contract.value
# names(ted.value)=c("file", "value.usd", "lcu.unit", "value.lcu")



load("22 TED scrape/result/Final data set.Rdata")
rm(ted.value)