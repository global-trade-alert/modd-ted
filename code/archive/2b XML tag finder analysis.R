library(XML)
library(stringr)
setwd("GTA cloud")
rm(list = ls())
load("22 TED scrape/result/xml tags.Rdata")


combinations=aggregate(file ~.,result.xml.tags, function(x) length(unique(x)))

combinations=merge(combinations, aggregate(file ~tag,result.xml.tags, function(x) length(unique(x))), by="tag", all.x=T)
names(combinations)=c("tag", "attribute", "count.ta", "count.t")

