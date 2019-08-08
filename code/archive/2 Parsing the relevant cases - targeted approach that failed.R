library(XML)
library(stringr)
library(splitstackshape)
library(IMFData)
library(webdriver)
setwd("GTA cloud")
rm(list = ls())

## helpful case
# https://ted.europa.eu/udl?uri=TED:NOTICE:209631-2018:TEXT:EN:HTML&src=0#id4-V.

## functions
get_attr=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x))), collapse=";")
}

get_value=function(document, attribute){
  paste(unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x))), collapse=";")
}

get_attr_vector=function(document, attribute){
  unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlAttrs(x)))
}

get_value_vector=function(document, attribute){
  unlist(lapply(xmlElementsByTagName(xmlRoot(document), attribute, recursive = T), function(x) xmlValue(x)))
}


## CPV values
cpv.names=read.csv("22 TED scrape/help files/cpv_names.csv", sep=";")
cpv.names$type="good"
cpv.names$type[cpv.names$cpv>=45000000]="service"

goods.cpvs=subset(cpv.names, type=="good")$cpv

## TED contract value tags
ted.value.tags=c("VALUE", "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", "VAL_TOTAL", "INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT", "COSTS_RANGE_AND_CURRENCY", "VAL_ESTIMATED_TOTAL", "VAL_OBJECT", "VAL_TOTAL_AFTER", "VAL_TOTAL_BEFORE", "DOCUMENT_COST", "PRICE_PAID", "EXCLUDING_VAT_VALUE", "VAL_RANGE_TOTAL", "VAL_BARGAIN_PURCHASE", "VALUE_RANGE", "LOW", "HIGH", "VAL_SUBCONTRACTING", "VAL_PRIZE", "PRIZE_VALUE",
      "RANGE_VALUE", "RANGE_VALUE_COST", "LOW_VALUE","HIGH_VALUE")

# IMF currency data
# IFS.available.codes <- DataStructureMethod('IFS') # Get dimension code of IFS dataset
# IFS.search=as.data.frame(IFS.available.codes[[2]]) # Possible code in the country dimension

imf.cur=data.frame(value.cur=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK", "JPY", "LTL", "LVL", "MTL"),
                   imf.symbol=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"),
                   stringsAsFactors = F)


databaseID <- 'IFS'
startdate='2008-11-01'
enddate=Sys.Date()
checkquery = FALSE


queryfilter <- list(CL_FREQ='M', CL_AREA_IFS=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"), CL_INDICATOR_IFS =c('ENDA_XDC_USD_RATE'))
fx.rate <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy = T)
fx.rate=fx.rate[,c(1,2,4)]
names(fx.rate)=c("y.m","divide.by", "imf.symbol")


## the list of files to parse
load("22 TED scrape/result/GTA TED replica.Rdata")

### ODD cases
odd.cases= c("269373-2017", "499380-2017", "179395-2017","204469-2016", "522405-2017","493261-2017", "349780-2017", "371754-2017" ,"443459-2014", "474378-2017", "474313-2017", "264859-2018", "150071-2016")


t.cases=subset(ted.cases, file.extracted==T & fully.parsed==F &!  case.id %in% odd.cases)
relevant.files=gsub("-","_",t.cases$case.id)


all.xml=list.files(path = "22 TED scrape/relevant cases", pattern = ".xml",  full.names = T, recursive = T)
all.xml=all.xml[gsub("^0+","",str_extract(str_extract(all.xml, "\\d+_\\d+.xml"), "\\d+_\\d+")) %in% relevant.files]



# loop
step=0
n=length(all.xml)

# for checking whether source site still exists
pjs <- run_phantomjs()
remDr=Session$new(port=pjs$port)


while(length(all.xml)>0){
  doc=xmlParse(all.xml[1])
  file=all.xml[1]
  
  check.ids=unique(c(gsub("_","-",str_extract(file, "\\d+_\\d+")), gsub("^0+","",gsub("_","-",str_extract(file, "\\d+_\\d+")))))
  check.ids=check.ids[check.ids %in% ted.cases$case.id][1]
  
  t.cases$file[t.cases$case.id %in% check.ids]=file
  t.cases$gta.relevant[t.cases$case.id %in% check.ids]=T

  ## First order check for GTA relevance: GPA
  g="none found"
  if(length(xmlElementsByTagName(xmlRoot(doc), "CONTRACT_COVERED_GPA", recursive = T))>0){
    
    g=get_attr(doc, "CONTRACT_COVERED_GPA")
  } else{
    
    if(length(xmlElementsByTagName(xmlRoot(doc), "NO_CONTRACT_COVERED_GPA", recursive = T))>0){
      
      g="NO"
    }
    
    if(length(xmlElementsByTagName(xmlRoot(doc), "YES_CONTRACT_COVERED_GPA", recursive = T))>0){
      
      g="YES"
    }
    
  }
  
  t.cases$gpa[t.cases$case.id %in% check.ids]=g

  
  
  
  ## completing ted.case
  ## multi doc languages
  if(length(xmlElementsByTagName(xmlRoot(doc), "ML_TI_DOC", recursive = T)) >0){
    t.cases$multi.doc[t.cases$case.id %in% check.ids]=get_attr(doc, "ML_TI_DOC")
    multi=get_attr_vector(doc, "ML_TI_DOC")
    en.present=sum(as.numeric(multi=="EN"))
  }
  
  
  ## tender source
  if(length(xmlElementsByTagName(xmlRoot(doc), "URI_DOC", recursive = T)) >0){
    multi.url=get_attr_vector(doc, "URI_DOC")
    en.url.present=sum(as.numeric(multi.url=="EN"))
    
    if(en.url.present>0){
      t.src=get_value_vector(doc, "URI_DOC")[which(multi.url=="EN")]
    } else {
      t.src=get_value(doc, "URI_DOC")
    }
    
    ## check sources
    remDr$go(t.src)
    if(remDr$getUrl()=="https://ted.europa.eu/TED/error-pages/expiredUDLLink.do"){
      t.src="https://ted.europa.eu/TED/search/search.do"
    }
    
    t.cases$tender.source[t.cases$case.id %in% check.ids]=t.src
    rm(t.src)
  }
  
  # issuer name
  if(length(xmlElementsByTagName(xmlRoot(doc), "AA_NAME", recursive = T)) >0){
    if(sum(as.numeric(get_value_vector(doc, "AA_NAME")=="EN"))>0){
      t.cases$issuer.name[t.cases$case.id %in% check.ids]=get_value_vector(doc, "AA_NAME")[which(get_value_vector(doc, "AA_NAME")=="EN")]
    } else {
      t.cases$issuer.name[t.cases$case.id %in% check.ids]=get_value_vector(doc, "AA_NAME")[1]
    }
  }
  
  
  ## Tender town
  if(length(xmlElementsByTagName(xmlRoot(doc), "TI_TOWN", recursive = T)) >0){
    if(en.present>0){
      t.cases$issuer.town[t.cases$case.id %in% check.ids]=get_value_vector(doc, "TI_TOWN")[which(multi=="EN")]
    } else {
      t.cases$issuer.town[t.cases$case.id %in% check.ids]=get_value(doc, "TI_TOWN")
    }
    
  }
  
  # tender country
  if(length(xmlElementsByTagName(xmlRoot(doc), "ISO_COUNTRY", recursive = T)) >0){
    t.cases$issuer.country[t.cases$case.id %in% check.ids]=get_attr(doc, "ISO_COUNTRY")
  }
  
  
  ## cleaning unicode names
  t.cases$issuer.name=gsub(">",";",gsub("(<.*?)(\\d+)","&#1\\2",enc2native(t.cases$issuer.name)))
  t.cases$issuer.name[grepl("[0-9][A-Z];", t.cases$issuer.name)]=tolower(t.cases$issuer.name[grepl("[0-9][A-Z];", t.cases$issuer.name)])
  
  
  ## Complete ted.directive
  if(length(xmlElementsByTagName(xmlRoot(doc), "DIRECTIVE", recursive = T)) >0){
    t.directive=data.frame(case.id=check.ids, 
                           directive=get_attr(doc, "DIRECTIVE"), 
                           stringsAsFactors = F)
    rm(t.directive)
  }
  
  
  
  
  
  # publication date stated inside the XML
  if(length(xmlElementsByTagName(xmlRoot(doc), "DATE_PUB", recursive = T)) >0){
    dt=as.Date(get_value(doc, "DATE_PUB"), "%Y%m%d")
  }
  
  
  
  ## CPV code values
  ## ony those in goods are relevant.
  
  t.cpv=data.frame(case.id=check.ids, cpv.tag=NA, cpv.value=NA, stringsAsFactors = F)
  
  if(length(xmlElementsByTagName(xmlRoot(doc), "CPV_CODE", recursive = T))>0){
    t.cpv=data.frame(case.id=check.ids, 
                     cpv.tag="CPV_CODE", 
                     cpv.value=get_attr(doc, "CPV_CODE"), 
                     stringsAsFactors = F)
    
  } else {
    if(length(xmlElementsByTagName(xmlRoot(doc), "ORIGINAL_CPV", recursive = T))>0){
      t.cpv=data.frame(case.id=check.ids, 
                       cpv.tag="ORIGINAL_CPV", 
                       cpv.value=get_attr(doc, "ORIGINAL_CPV"), 
                       stringsAsFactors = F)
    }
  }
  
  
  ## CPV headline
  if(length(xmlElementsByTagName(xmlRoot(doc), "TI_TEXT", recursive = T)) >0){
    if(en.present>0){
      t.cpv$cpv.title=get_value_vector(doc, "TI_TEXT")[which(multi=="EN")]
    } else {
      t.cpv$cpv.title=get_value(doc, "TI_TEXT")
    }
  } else {
    t.cpv$cpv.title=NA
  }
  
  
  ## Only goods CPVs are GTA-relevant
  if(sum(as.numeric(unique(cSplit(t.cpv, which(names(t.cpv)=="cpv.value"), sep=";", direction = "long")$cpv.value) %in% goods.cpvs))==0){
    t.cases$gta.relevant[t.cases$case.id %in% check.ids]=F
  }
  
  
  ################# START FROM HERE
  
  ## contract values 
  ## only those above USD 10 million are relevant
  
  ## extracting the values
  t.value=data.frame(case.id=character(), date=character(), value.tag=character(), value.cur=character(), value.value=character(), 
                     numeric=character(), value.usd=character(), stringsAsFactors = F)
  for(v in ted.value.tags){
    if(length(xmlElementsByTagName(xmlRoot(doc), v, recursive = T))>0){
      
      
      if(sum(as.numeric(is.na(str_extract(names(unlist(lapply(xmlElementsByTagName(xmlRoot(doc), v, recursive = T), function(x) xmlChildren(x)))), "text$"))==F))>0){
        
        t.value=rbind(t.value, data.frame(case.id=check.ids, date=dt, 
                           value.tag=v, 
                           value.cur=paste(get_attr_vector(doc, v)[grepl("CURRENCY",names(get_attr_vector(doc, v)))], collapse=";"), 
                           value.value=unlist(lapply(unlist(lapply(xmlElementsByTagName(xmlRoot(doc), v, recursive = T), function(x) xmlChildren(x))), function(x) xmlValue(x)))[is.na(str_extract(names(unlist(lapply(xmlElementsByTagName(xmlRoot(doc), v, recursive = T), function(x) xmlChildren(x)))), "text$"))==F], 
                           numeric=NA, value.usd=NA, stringsAsFactors = F)
                )
        
        
      } else {
        
        t.value=rbind(t.value, data.frame(case.id=check.ids, date=dt,  
                           value.tag=v, 
                           value.cur=paste(get_attr_vector(doc, v)[grepl("CURRENCY",names(get_attr_vector(doc, v)))], collapse=";"), 
                           value.value=unlist(lapply(unlist(lapply(xmlElementsByTagName(xmlRoot(doc), v, recursive = T), function(x) xmlChildren(x))), function(x) xmlValue(x))), 
                           numeric=NA, value.usd=NA, stringsAsFactors = F)
        )
       
      }
      
      
    }
    
  }
  rownames(t.value)=NULL
  
  
  
  ## cleaning the values
 if(nrow(t.value)>0){
   
   t.value$numeric=gsub("([A-Z]+: )|(: )","", t.value$value.value)
   t.value$numeric=gsub(",\\d{2}",";", t.value$numeric)
   
   t.value=cSplit(t.value, which(names(t.value)=="numeric"), direction="long", sep=";")
   
   non.split=subset(t.value, is.na(str_extract(numeric, " \\d{4,} "))==F)
   non.split$odd.values=str_extract(non.split$numeric, " \\d{4,} ")
   non.split=subset(non.split, is.na(odd.values)==F)
   non.split$odd.values=gsub("(^\\d{3})(\\d+)","\\1;\\2", gsub(" ","",non.split$odd.values))
   non.split$numeric=apply(non.split, 1, function(x) gsub(" \\d{4,} ",x[which(names(non.split)=="odd.values")],x[which(names(non.split)=="numeric")]))
   non.split$odd.values=NULL
   
   if(nrow(non.split)>0){
     non.split=cSplit(non.split, which(names(non.split)=="numeric"), direction="long", sep=";")
     t.value=rbind(subset(t.value, !case.id %in% non.split$case.id),non.split)
     
   }
   rm(non.split)
   
   t.value$numeric=as.numeric(gsub(" ","",t.value$numeric))
   t.value$used.by.gta=F
   t.value$do.random.checks=T
   
   if(nrow(subset(t.value, grepl("COSTS_RANGE_AND_CURRENCY", value.tag, ignore.case = T) &
                  grepl("[A-Z]{3}", value.cur, ignore.case = T) &
                  numeric>100))>0){
     tv=subset(t.value, grepl("COSTS_RANGE_AND_CURRENCY", value.tag, ignore.case = T) &
                 grepl("[A-Z]{3}", value.cur, ignore.case = T) & numeric>100)
     
     t.value$used.by.gta[t.value$numeric==min(tv$numeric, na.rm = T) & 
                           grepl("[A-Z]{3}", t.value$value.cur, ignore.case = T)] =T
     
     t.value$do.random.checks=F
     
     
   } else {
     if(nrow(subset(t.value, grepl("LOW_VALUE", value.tag, ignore.case = T) &
                    grepl("[A-Z]{3}", value.cur, ignore.case = T) &
                    numeric>100))>0){
       
       tv=subset(t.value, grepl("COSTS_RANGE_AND_CURRENCY", value.tag, ignore.case = T) &
                   grepl("[A-Z]{3}", value.cur, ignore.case = T) & numeric>100)
       
       t.value$used.by.gta[t.value$numeric==min(tv$numeric, na.rm = T) & 
                             grepl("[A-Z]{3}", t.value$value.cur, ignore.case = T)] =T
       
       t.value$do.random.checks=F
     } else {
       
       if(nrow(subset(t.value, grepl("[A-Z]{3}", value.cur, ignore.case = T) & numeric>100))>0){
         tv=subset(t.value, grepl("[A-Z]{3}", value.cur, ignore.case = T) & numeric>100)
         
         t.value$used.by.gta[t.value$numeric==min(tv$numeric, na.rm = T) & 
                               grepl("[A-Z]{3}", t.value$value.cur, ignore.case = T)] =T
       }
       
     }
     
   }
   
   
   
   ## Conversion to USD
   t.value$y.m=paste(sprintf("%02i",year(dt)), sprintf("%02i",month(dt)), sep="-")
   if(nrow(subset(t.value, used.by.gta==T))>0){
     tv=subset(t.value, used.by.gta==T & grepl("[A-Z]{3}",value.cur))
     
     for(i in 1:nrow(tv)){
       tv$value.usd[i]=tv$numeric[i]/as.numeric(fx.rate$divide.by[fx.rate$y.m==tv$y.m[i] & 
                                                                    fx.rate$imf.symbol==unique(imf.cur$imf.symbol[imf.cur$value.cur==str_extract(tv$value.cur[i], "[A-Z]{3}")])])
     }
     
     t.value=rbind(subset(t.value, !(used.by.gta==T & grepl("[A-Z]{3}",value.cur))), tv)
     
   }
   
   
   if(check.ids=="190770-2014"){
     t.value$value.value=800000000
     t.value$value.usd=800000000/.62 
     t.value$used.by.gta=F
     t.value$used.by.gta[1]=T
     
   }
   
   t.value$exceeds.10m=t.value$value.usd>10000000
   
   ## GTA.relevant?  
   
   if(is.na(t.cases$gta.relevant[t.cases$case.id==check.ids])){
     
     if(length(unique(t.value$exceeds.10m)[is.na(unique(t.value$exceeds.10m))==F])>0){
       t.cases$gta.relevant=T
     } else {
       t.cases$gta.relevant=F
     }
     
     
   }  
   
 }
   
  
  ted.cpv=rbind(ted.cpv, t.cpv)
  ted.value=rbind(ted.value, t.value)
  ted.cases=rbind(subset(ted.cases, ! case.id %in% t.cases$case.id), t.cases)
  ted.directive=rbind(ted.directive, t.directive)
  
  ## updating status log
  
  print((n-length(all.xml))/n)
  step=step+1
  
  if(step>=500){

    # saving parsing results
    save(ted.cases,ted.directive,ted.cpv,ted.value, file="22 TED scrape/result/GTA TED replica.Rdata")

    # reset
    step=0
  }
  all.xml=all.xml[-1]
  print(step)
  rm(t.value,t.cpv)

}


save(ted.cases,ted.directive,ted.cpv,ted.value, file="22 TED scrape/result/GTA TED replica.Rdata")
