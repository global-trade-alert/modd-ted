library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
rm(list = ls())

setwd("GTA cloud")
load("22 TED scrape/result/TED processed cases.Rdata")

conversion.ids=as.character(subset(ted.processed, gta.relevant==T & fully.hs.converted==F)$id)

check all relevant cases, that have not been added to a bulk yet whether they need their CPV codes matched.
  