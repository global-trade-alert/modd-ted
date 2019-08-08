library(XML)
library(stringr)
library(splitstackshape)
library(IMFData)
setwd("GTA cloud")
rm(list = ls())
options("scipen"=999, "digits"=4)

load("22 TED scrape/result/TED results - latest search.Rdata")
results$file=str_extract(results$file, "\\d+_\\d+\\.xml")


# IMF currency values
imf.cur=data.frame(currency=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK", "JPY", "LTL", "LVL", "MTL"),
                   imf.symbol=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"),
                   stringsAsFactors = F)

databaseID <- 'IFS'
startdate='2008-11-01'
enddate=Sys.Date()
checkquery = FALSE


queryfilter <- list(CL_FREQ='M', CL_AREA_IFS=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"), CL_INDICATOR_IFS =c('ENDA_XDC_USD_RATE'))
ex.rate <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy = T)

ex.rate=ex.rate[,c(1,2,4)]
names(ex.rate)=c("date","divide.by", "imf.symbol")
ex.rate=merge(ex.rate, imf.cur, by="imf.symbol",all.x=T)
ex.rate$imf.symbol=NULL


## data to parse
parse.me=list.files(path = "22 TED scrape/result/parsed-not-processed", pattern = ".Rdata",  full.names = T, recursive = T)

p.round=1
gta.total=0
for(parse.now in parse.me){
  
  
  load(parse.now)
  
  results$fully.parsed[results$file %in% unique(ted.parsed.complete$file)]=T
  
  
  #### FIRST TEST: GPA excluded?
  ## cases that include a reference to GPA
  gpa.references=unique(ted.parsed.complete$file[(grepl("GPA", ted.parsed.complete$element.name)|grepl("GPA", ted.parsed.complete$element.value))])
  
  ##certainly out b/c covered by GPA or no reference to GPA
  gpa.out=unique(ted.parsed.complete$file[ ted.parsed.complete$element.name=="CONTRACT_COVERED_GPA"])
  gpa.out=unique(c(gpa.out,ted.parsed.complete$file[!ted.parsed.complete$file %in% gpa.references] ))
  
  ##certainly in b/c not covered by GPA
  gta.relevant=unique(ted.parsed.complete$file[(grepl("NO_CONTRACT_COVERED_GPA", ted.parsed.complete$element.name))])
  
  # should be zero
  intersect(gta.relevant, gpa.out)
  
  gpa.maybe=setdiff(gpa.references,c(gta.relevant,gpa.out))
  
  gpa.elements=unique(ted.parsed.complete$element.name[(grepl("GPA", ted.parsed.complete$element.name)|grepl("GPA", ted.parsed.complete$element.value))  & 
                                                         ted.parsed.complete$file %in% gpa.maybe])
  
  see.gpa=subset(ted.parsed.complete, file %in% gpa.maybe & element.name %in% gpa.elements)
  
  ## were affirmative in first check
  unique(see.gpa$element.value[see.gpa$element.name=="RP_REGULATION"])
  
  gpa.participation=unique(see.gpa$file[see.gpa$element.name=="RP_REGULATION" & grepl("with participation.*?GPA",see.gpa$element.value, ignore.case = T)])
  gpa.out=unique(c(gpa.out, gpa.participation))
  
  if(length(unique(ted.parsed.complete$file))!=length(unique(c(gpa.out, gta.relevant)))){
    load("22 TED scrape/result/TED parsing error log.Rdata")
    
    gpa.unsolved=setdiff(unique(ted.parsed.complete$file), c(gpa.out, gta.relevant))
    ted.error.log=rbind(ted.error.log, 
                        data.frame(file=gpa.unsolved,
                                   issue="GPA status unclear",
                                   batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                   stringsAsFactors = F))
    
    save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
    
    gta.relevant=setdiff(gta.relevant, gpa.unsolved)
    rm(ted.error.log, gpa.unsolved)
    
  }
  
  ## first cut
  print(paste("Number of cases after GPA cut: ", length(gta.relevant), sep=""))
  ted.gta.relevant=subset(ted.parsed.complete, file %in% gta.relevant)
  ted.gta.relevant$element.value[nchar(ted.gta.relevant$element.value)==0]=NA
  
  #### Second relevance test: CPV values included?
  ## some mentioning of CPV
  gta.relevant=intersect(gta.relevant,
                         unique(ted.gta.relevant$file[(grepl("CPV", ted.gta.relevant$element.name)|grepl("CPV", ted.gta.relevant$element.value)) & 
                                                        is.na(ted.gta.relevant$element.value)==F]))
  
  cpv.elements=unique(ted.gta.relevant$element.name[grepl("CPV", ted.gta.relevant$element.name)|grepl("CPV", ted.gta.relevant$element.value)])
  cpv.elements.standard=c("ORIGINAL_CPV","CPV_MAIN","CPV_CODE","CPV_ADDITIONAL","CPV_SUPPLEMENTARY_CODE")
  
  cpv.easy=unique(ted.gta.relevant$file[ted.gta.relevant$element.name %in% cpv.elements.standard])
  
  ted.cpv=data.frame(file=character(),
                     cpv.code=character(),
                     stringsAsFactors = F)
  for(case in cpv.easy){
    cpv.temp=subset(ted.gta.relevant, file==case)
    cpv.positions=unique(subset(cpv.temp, element.name %in% cpv.elements.standard)$position)
    
    ted.cpv=rbind(ted.cpv, 
                  data.frame(file=case,
                             cpv.code=unique(subset(cpv.temp, position %in% cpv.positions & 
                                                      is.attribute==T &
                                                      is.na(element.value)==F)$element.value),
                             stringsAsFactors = F))
    rm(cpv.temp, cpv.positions)
    
  }
  
  if(length(cpv.easy)!=length(gta.relevant)){
    load("22 TED scrape/result/TED parsing error log.Rdata")
    
    cpv.unsolved=setdiff(gta.relevant, cpv.easy)
    ted.error.log=rbind(ted.error.log, 
                        data.frame(file=cpv.unsolved,
                                   issue="CPV values unclear",
                                   batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                   stringsAsFactors = F))
    
    save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
    
    gta.relevant=setdiff(gta.relevant, cpv.unsolved)
    rm(ted.error.log, cpv.unsolved)
    
    
  }
  
  ## cleaning out odd values
  ted.cpv=subset(ted.cpv, grepl("\\D+", cpv.code)==F)
  
  ## Restricting to those with at least one non-service sector code
  gta.relevant=intersect(gta.relevant, unique(subset(ted.cpv, as.numeric(as.character(cpv.code))<45000000)$file))
  ted.cpv=subset(ted.cpv, file %in% gta.relevant)
  
  
  ## second cut: Pure service-sector cases
  print(paste("Number of cases after CPV cut: ", length(gta.relevant), sep=""))
  ted.gta.relevant=subset(ted.gta.relevant, file %in% gta.relevant)
  
  
  ### THIRD TEST: Contract value
  ted.currency=unique(subset(ted.gta.relevant, element.name=="CURRENCY" & is.attribute==T)$file)
  ted.contract.values=unique(subset(ted.gta.relevant, element.name=="VALUE" & is.attribute==F & is.na(element.value)==F)$file)
  
  ted.contract.values=unique(c(ted.contract.values,
                               unique(subset(ted.gta.relevant, element.name=="VAL_OBJECT")$file)))
  
  ted.contract.values=unique(c(ted.contract.values,
                               unique(subset(ted.gta.relevant, element.name=="VAL_ESTIMATED_TOTAL")$file)))
  
  gta.relevant=intersect(gta.relevant, 
                         ted.contract.values)
  
  # third cut: remove all cases without contract value statements
  print(paste("Number of cases after contract value cut: ", length(gta.relevant), sep=""))
  ted.gta.relevant=subset(ted.gta.relevant, file %in% gta.relevant)
  
  
  
  ### FOURTH TEST: Contract value above USD 10 million
  
  ted.value=data.frame(file=character(),
                       date=character(),
                       lcu.value=character(),
                       currency=character(),
                       value.type=character(),
                       stringsAsFactors = F)
  
  for(case in gta.relevant){
    value.temp=subset(ted.gta.relevant, file==case)
    date.temp=as.Date(value.temp$element.value[value.temp$element.name=="DATE_PUB"], "%Y%m%d")
    
    value.positions=unique(subset(value.temp, element.name %in% "CURRENCY")$position)
    # value.temp=subset(value.temp, position %in% value.positions)
    
    missing.contract.value=T
    found.something=F
    
    proc.positions=unique(value.temp$position[value.temp$element.value=="PROCUREMENT_TOTAL"])
    proc.positions=proc.positions[is.na(proc.positions)==F]
    
    total.positions=unique(subset(value.temp, element.name %in% c("ESTIMATED_TOTAL","VAL_ESTIMATED_TOTAL"))$position)
    total.positions=total.positions[is.na(total.positions)==F]
    total.positions=setdiff(total.positions, proc.positions)
    
    aux.positions=setdiff(value.positions, total.positions)
    aux.positions=aux.positions[is.na(aux.positions)==F]
    
    if(length(proc.positions)>0){ 
      for(pos in proc.positions){
        
        if(length(value.temp$position[value.temp$element.name=="LOW" & grepl(pos, value.temp$position)])>0){
          lcu.temp=min(value.temp$element.value[value.temp$element.name=="LOW" & grepl(pos, value.temp$position)], na.rm = T)
        } else {
          lcu.temp=value.temp$element.value[value.temp$position==pos & value.temp$is.attribute==F]
        }
        
        ted.value=rbind(ted.value,
                        data.frame(file=case,
                                   date=date.temp,
                                   lcu.value=lcu.temp,
                                   currency=value.temp$element.value[value.temp$position==pos & value.temp$element.name=="CURRENCY"],
                                   type="total"))
                
        missing.contract.value=F    
        rm(lcu.temp)
        
      }
    }
    
    if(missing.contract.value){
      if(length(total.positions)>0){ 
        for(pos in total.positions){
          
          if(length(value.temp$position[value.temp$element.name=="LOW" & grepl(pos, value.temp$position)])>0){
            lcu.temp=min(value.temp$element.value[value.temp$element.name=="LOW" & grepl(pos, value.temp$position)], na.rm = T)
          } else {
            lcu.temp=value.temp$element.value[value.temp$position==pos & value.temp$is.attribute==F]
          }
          
          
          ted.value=rbind(ted.value,
                          data.frame(file=case,
                                     date=date.temp,
                                     lcu.value=lcu.temp,
                                     currency=value.temp$element.value[value.temp$position==pos & value.temp$element.name=="CURRENCY"],
                                     type="total"))
                  
          missing.contract.value=F  
          rm(lcu.temp)
          
        }
      }
    }
    
    
    if(missing.contract.value){
      if(length(aux.positions)>0){
        for(pos in aux.positions){
          
          if(length(value.temp$position[value.temp$element.name=="LOW" & grepl(pos, value.temp$position)])>0){
            lcu.temp=min(value.temp$element.value[value.temp$element.name=="LOW" & grepl(pos, value.temp$position)], na.rm = T)
          } else {
            lcu.temp=value.temp$element.value[value.temp$position==pos & value.temp$is.attribute==F]
          }
          
          ted.value=rbind(ted.value,
                          data.frame(file=case,
                                     date=date.temp,
                                     lcu.value=lcu.temp,
                                     currency=value.temp$element.value[value.temp$position==pos & value.temp$element.name=="CURRENCY"],
                                     type="aggregated"))
                  
          missing.contract.value=F 
          rm(lcu.temp)
          
        }
        
      }
      
    }
    
    
    
    
    if(missing.contract.value){
      load("22 TED scrape/result/TED parsing error log.Rdata")
      
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=case,
                                     issue="No contract value",
                                     batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
      
      gta.relevant=setdiff(gta.relevant, case)
      ted.currency=setdiff(ted.currency, case)
      rm(ted.error.log)
    }
    
    rm(value.temp,value.positions, aux.positions, total.positions)
  }
  
  if(length(setdiff(gta.relevant, ted.currency))>0){
    load("22 TED scrape/result/TED parsing error log.Rdata")
    
    value.unsolved=setdiff(gta.relevant, ted.currency)
    ted.error.log=rbind(ted.error.log, 
                        data.frame(file=value.unsolved,
                                   issue="No currency tag",
                                   batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                   stringsAsFactors = F))
    
    save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
    
    gta.relevant=setdiff(gta.relevant, value.unsolved)
    rm(ted.error.log, value.unsolved)
    
    
  }
  
  ## Value cleaning
  # removing cases with non-digits
  ted.value=subset(ted.value, grepl("\\D+", gsub(",|\\.","",lcu.value))==F)
  ted.value$lcu.value=gsub("\\.\\d{1,2}",";",ted.value$lcu.value)
  ted.value=cSplit(ted.value, which(names(ted.value)=="lcu.value"), sep=";", direction="long")
  ted.value$lcu.value=as.numeric(as.character(ted.value$lcu.value))
  
  ted.value=aggregate(lcu.value ~ file + currency + type + date, ted.value, sum)
  
  
  ted.value$date=paste(year(ted.value$date),sprintf("%02i",month(ted.value$date)),sep="-")
  
  ted.value=merge(ted.value, ex.rate, by=c("date", "currency"), all.x=T)
  
  missing.value=subset(ted.value, is.na(divide.by))
  ted.value=subset(ted.value, is.na(divide.by)==F)
  
  if(nrow(missing.value)>0){
    missing.value$divide.by=NULL
    missing.value$date=paste(strsplit(missing.value$date,"-")[[1]][1], 
                             sprintf("%02i",as.numeric(strsplit(missing.value$date,"-")[[1]][2])-1), sep="-")
    
    missing.value=merge(missing.value, ex.rate, by=c("date", "currency"), all.x=T)
    
    ted.value=rbind(ted.value, missing.value)
    rm(missing.value)
  }
  
  if(nrow(subset(ted.value, is.na(divide.by)))>0){
    
    load("22 TED scrape/result/TED parsing error log.Rdata")
    
    value.unsolved=setdiff(gta.relevant, ted.currency)
    ted.error.log=rbind(ted.error.log, 
                        data.frame(file=value.unsolved,
                                   issue="No currency tag",
                                   batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                   stringsAsFactors = F))
    
    save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
    
    gta.relevant=setdiff(gta.relevant, value.unsolved)
    rm(ted.error.log, value.unsolved)
    
    
  }
  ted.value$usd.value=ted.value$lcu.value/as.numeric(as.character(ted.value$divide.by))
  
  gta.total=gta.total+length(unique(subset(ted.value, usd.value>=10000000)$file))
  print(paste("This batch includes ",length(unique(subset(ted.value, usd.value>=10000000)$file))," contracts above USD 10 mn.", sep=""))
  
  
  
  ### Adding further variables
  ted.base=data.frame()
  for(case in gta.relevant){
    
    ted.temp=subset(ted.gta.relevant, file==case)
    
    ## Date (annoucement & implementation).
    d.announce=as.Date(ted.temp$element.value[ted.temp$element.name=="DATE_PUB"], "%Y%m%d")
    if(length(d.announce)==0){
      d.announce=NA
      
      load("22 TED scrape/result/TED parsing error log.Rdata")
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=case,
                                     issue="No announcement date",
                                     batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
      
      rm(ted.error.log)
    }
    
    d.implemented=ted.temp$element.value[ted.temp$element.name=="DATE_OPENING_TENDERS"]
    if(length(d.implemented)==0){d.implemented=d.announce}
    
    duration.pos=ted.temp$position[ted.temp$element.name=="DURATION_TENDER_VALID"]
    duration=character()
    if(length(duration.pos)>0){
      duration=character()
      
      for(d.pos in duration.pos){
        duration=c(duration,
                   paste(ted.temp$element.value[ted.temp$position==d.pos], collapse=";"))
      }
      
      if(length(duration)==0){duration=NA} else {
        duration=paste(unique(duration), collapse=";")
      }
      
    }
    if(length(duration)==0){duration=NA}
    
    ## Directive
    dir.pos=ted.temp$position[ted.temp$element.name=="LEGAL_BASIS"]
    directive=paste(unique(ted.temp$element.value[ted.temp$position %in% dir.pos & ted.temp$is.attribute==T]  ), collapse=";")
    if(length(directive)==0){
      directive=NA
      
      load("22 TED scrape/result/TED parsing error log.Rdata")
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=case,
                                     issue="No directive",
                                     batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
      
      rm(ted.error.log)
    }
    
    ## Implementing jurisdiction
    ij.pos=ted.temp$position[ted.temp$element.name=="ISO_COUNTRY"]
    ij=paste(unique(ted.temp$element.value[ted.temp$position %in% ij.pos & ted.temp$is.attribute==T]), collapse=";")
    
    if(length(ij)==0){
      ij=NA
      
      load("22 TED scrape/result/TED parsing error log.Rdata")
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=case,
                                     issue="No IJ",
                                     batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
      
      rm(ted.error.log)
    }
    
    ## Tender implementation
    ## issuing authority
    tender.authority=paste(ted.temp$element.value[ted.temp$element.name=="AA_AUTHORITY_TYPE"], collapse=";")
    
    tender.nature=paste(ted.temp$element.value[ted.temp$element.name=="NC_CONTRACT_NATURE"], collapse=";")
    
    issuer.pos=min(ted.temp$position[ted.temp$element.name=="AA_NAME"])
    
    tender.issuer=paste(unique(ted.temp$element.value[ted.temp$position %in% issuer.pos & ted.temp$element.name=="AA_NAME"]), collapse=";")
    tender.issuer.lang=paste(unique(ted.temp$element.value[ted.temp$position %in% issuer.pos & ted.temp$element.name=="LG"]), collapse=";")
    
    if(length(tender.issuer)==0){
      tender.issuer=NA
      tender.issuer.lang=NA
      
      load("22 TED scrape/result/TED parsing error log.Rdata")
      
      ted.error.log=rbind(ted.error.log, 
                          data.frame(file=case,
                                     issue="No tender issuer",
                                     batch=gsub("22 TED scrape/result/parsed-not-processed/","",parse.now),
                                     stringsAsFactors = F))
      
      save(ted.error.log, file="22 TED scrape/result/TED parsing error log.Rdata")
      
      rm(ted.error.log)
    }
    
    
    tender.doc.type=ted.temp$element.value[ted.temp$element.name=="TD_DOCUMENT_TYPE"]
    if(length(tender.doc.type)==0){tender.doc.type=NA}
    
    if(length(ted.temp$position[ted.temp$element.name=="LG" & ted.temp$element.value=="EN"])>0){
      ti.pos=ted.temp$position[ted.temp$element.name=="LG" & ted.temp$element.value=="EN"]
    } else {
      orig.lg=ted.temp$element.value[ted.temp$element.name=="LG_ORIG"]
      ti.pos=ted.temp$position[ted.temp$element.name=="LG" & ted.temp$element.value==ti.pos]
    }
    
    if(length(ti.pos)>0){
      tender.town=character()
      tender.content=character()
      
      for(tp in ti.pos){
        
        tender.town=c(tender.town,
                      ted.temp$element.value[ted.temp$element.name=="TI_TOWN" & grepl(tp, ted.temp$position)])
        tender.content=c(tender.content,
                         ted.temp$element.value[ted.temp$element.name=="TI_TEXT" & grepl(tp, ted.temp$position)])
        
        
      }
      
      if(length(tender.town)==0){tender.town=NA} else {
        tender.town=paste(unique(tender.town), collapse=";")
      }
      
      if(length(tender.content)==0){tender.content=NA}else {
        tender.content=paste(unique(tender.content), collapse=";")
      }
      
    }
    
    
    
    ## beneficiary
    
    contractor.country=character()
    contractor.name=character()
    contractor.town=character()
    
    con.pos=ted.temp$position[ted.temp$element.name=="CONTRACTOR"]
    if(length(con.pos)>0){
 
      for(i in 1:length(con.pos)){
        
        for(c in con.pos){
          
          
          contractor.name=c(contractor.name,
                            ted.temp$element.value[ted.temp$element.name=="OFFICIALNAME" & grepl(c, ted.temp$position)])
          
          contractor.town=c(contractor.town,
                            ted.temp$element.value[ted.temp$element.name=="TOWN" & grepl(c, ted.temp$position)])
          
          cty.pos=ted.temp$position[ted.temp$element.name=="COUNTRY" & 
                                      grepl(c, ted.temp$position)]
          
          contractor.country=c(contractor.country, unique(c(contractor.country, ted.temp$element.value[ted.temp$is.attribute==T &
                                                                                                         grepl(cty.pos, ted.temp$position)])))
        }
        
      }
      
      contractor.name=paste(unique(contractor.name), collapse=";")
      contractor.town=paste(unique(contractor.town), collapse=";")
      contractor.country=paste(unique(contractor.country), collapse=";")
    }
    
    if(length(contractor.name)==0){contractor.name=NA}
    if(length(contractor.town)==0){contractor.town=NA}
    if(length(contractor.country)==0){contractor.country=NA}
    
    ted.base=rbind(ted.base,
                   data.frame(file=case,
                              batch=parse.now, 
                              directive=directive,
                              implementing.jurisdiction=ij,
                              date.announced=d.announce,
                              date.implemented=d.implemented,
                              duration=duration,
                              issuer.name=tender.issuer,
                              issuer.name.language=tender.issuer.lang,
                              tender.authority=tender.authority,
                              tender.town=tender.town,
                              tender.nature=tender.nature,
                              tender.content=tender.content,
                              tender.doc.type=tender.doc.type,
                              contractor.name=contractor.name,
                              contractor.town=contractor.town,
                              contractor.country=contractor.country,
                              stringsAsFactors = F))
    
    
  }
  
  ## done
  load("22 TED scrape/result/GTA TED database.Rdata")
  ted.gta.base=rbind(ted.gta.base, subset(ted.base, ! file %in% ted.gta.base$file))
  ted.gta.cpv=rbind(ted.gta.cpv, subset(ted.cpv, ! file %in% ted.gta.cpv$file))
  ted.gta.value=rbind(ted.gta.value, subset(ted.value, ! file %in% ted.gta.value$file))
  
  ted.gta.value=subset(ted.gta.value, file %in% ted.gta.base$file)
  ted.gta.cpv=subset(ted.gta.cpv, file %in% ted.gta.base$file)
  
  save(ted.gta.base, ted.gta.cpv, ted.gta.value, file="22 TED scrape/result/GTA TED database.Rdata")
  
  
  rm(ted.parsed.complete)
  ## copy and remove
  file.copy(from=parse.now, to=gsub("parsed-not-processed","parsed-and-processed",parse.now), overwrite = T)
  file.remove(parse.now)
  
  print(paste("Finished parse ",p.round," out of ", length(parse.me),".", sep=""))
  print(paste("GTA-relevant total sofar: ",gta.total, sep=""))
  p.round=p.round+1
  
}


save(results, file="22 TED scrape/result/TED results - latest search.Rdata")


