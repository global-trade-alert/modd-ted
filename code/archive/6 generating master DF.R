library("rvest")
library("httr")
library(RCurl)
library(XML)
library("data.table")
library(xlsx)
library("stringr")
rm(list = ls())

setwd("GTA cloud")


## prep files
cpv.names=read.csv("22 TED scrape/help files/cpv_names.csv",sep=";")
directive.block=data.frame(directive=c("2004/17/EC","2014/25/EU"), text=c("The tender was issued under European Directive 'coordinating the procurement procedures of entities operating in the water, energy, transport and postal services sectors' (2004/17/EC). This Directive allows European government entities to discriminate against bidders from third countries.<br>According to Art. 58 paragraph 2 of 2004/17/EC, bids where the total product value sourced from third countries exceeds 50 percent 'may be rejected'. This option is further enforced in paragraph 3 of the same Article which states that in the case of two or more equivalent bids, preference 'shall' be given to one where the total product value sourced from third countries is below 50 percent.<br>In the context of this Directive, third countries are defined as countries outside the EU/EFTA area that have not signed 'an agreement ensuring comparable and effective access' to its public procurement system (Art. 58 paragraph 1).<br>The described procurement restriction in 2004/17/EC only covers the procurement of goods and excludes services. Software used in telecommunications network equipment is included as a product for the sake of this article.",
                                                                          "The tender was issued under European Directive 'on procurement by entities operating in the water, energy, transport and postal services sectors' (2014/25/EU). This Directive allows European government entities to discriminate against bidders from third countries.<br>According to Art. 83 paragraph 2 of 2014/25/EU, bids where the total product value sourced from third countries exceeds 50 percent 'may be rejected'. This option is further enforced in paragraph 3 of the same Article which states that in the case of two or more equivalent bids, preference 'shall' be given to one where the total product value sourced from third countries is below 50 percent.<br>In the context of this Directive, third countries are defined as countries outside the EU/EFTA area that have not signed 'an agreement ensuring comparable and effective access' to its public procurement system (Art. 85 paragraph 1).<br>The described procurement restriction in 2014/25/EU only covers the procurement of goods and excludes services. Software used in telecommunications network equipment is included as a product for the sake of this article."))

gov.entity=data.frame(issuer.country=c("UK"),
                      adjective=c("British"))

## TED data
load("22 TED scrape/result/Final data set.Rdata")
master=final

# for SA CSV
setnames(master, "id","import_id")
master$author_id=99
master$title=NA
master$announcement_description=NA
setnames(master, "date", "announcement_date")
master$source=NA
master$is_source_official=1
master$status=4

## for intervention CSV
,,,,,,,,,unit,tariff_peak,affected_sectors,IJ,DM,AJ
master$import_intervention_id=NA
master$intervention_type_id=NA
master$intervention_description=NA
master$affected_flow=1
master$evaluation_id=1
master$research_evaluation_id=1
master$inception_date=NA
master$removal_date=NA
master$is_duration_limited=0
master$eligible_firms_id=3
master$implementation_level_id=5
master$is_non_trade_related_rationale=0
master$is_jumbo=0
master$is_chain_measure=0
master$is_horizontal_measure=0
master$tariff_peak=0
master$is_fta_included=1
master$unit=2
master$IJ=NA
master$prior_level=0
master$new_level=NA


master$prior_level=0
master$new_level=NA

## for ATL CSV
import_intervention_id,tariff_line_code,prior_level,new_level,tariff_peak,is_tariff_line_official,sector_code


for(i in 1:nrow(master)){
  ## SA
  # Title
  master$title[i]
  
  ## announcement description
  master$announcement_description[i]
  
  # Source
  master$source[i]=paste("EU Tenders Electronic Daily (TED). Document number ", final$tender.number[i],". Available at ", final$tender.source[i], sep="")
  
  
  ## INT
  master$IJ[i]
  master$import_intervention_id[i]
  master$intervention_type_id[i]
  
  # Intervention description
  description.text=paste("On ",master$date[i],", the ",gov.entity$adjective[gov.entity$issuer.country==master$issuer.country[i]],
                         " government entity '",master$issuer.name[i],"' located in ",master$issuer.town[i],
                         " issued a tender including '",master$cpv.title[i],"' worth approximately ",master$lcu.unit[i]," ",format(round(master$value.lcu[i]/1000000,2), big.mark="'")," million (ca. USD ",format(round(master$value.usd[i]/1000000,2), big.mark="'"),
                         " million). In its terms, ",master$issuer.name[i]," states that this tender does not fall under the rules of the Agreement on General Procurement (GPA). The tender may have discriminated against bidders outside the EU/EFTA area for the reasons explained below.",sep="")
  
  ## add UL with CPVs here
  master$intervention_description[i]=paste(description.text, directive.block$text[directive.block$directive==master$directive[i]], sep="")
  rm(description.text)
  
  master$new_level[i]=master$value.usd[i]
  
  #Inception date
  master$inception_date
  
  
  
}






# implementing jurisdiction= {UK}
# affected jurisdiction= import origins outside EU/EFTA
# implementation level = subnational
# eligible firms = firm-specific
# prior/new value: 0 / {USD-equivalent of 32000000CHF}
# Announcement date = inception date = {23/12/2017}
# GTA evaluation = Amber
# HS codes/CPC sectors = {those corresponding to the listed CPV codes}"
# intervention.description=paste(,sep="")



save(master, "22 TED scrape/result/GTA data frame - 20180907.Rdata")
  