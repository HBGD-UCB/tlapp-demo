



rm(list=ls())
library(dplyr)
library(tidyr)
library(SuperLearner)
library(caret)


setwd("U:/data/Rally4b_data")


bindGHAP<-function(study, varlist=NULL, dynamicvars=NULL, d=d){
  
staticvars<-varlist[-which(varlist %in% dynamicvars)]

  study.d<-readRDS(paste0(study,".rds"))

  study.d <- study.d %>% filter(!is.na(AGEDAYS) & !is.na(HAZ))
  
  if(!is.null(varlist)){
    study.d<-study.d[,which(colnames(study.d) %in% varlist)]
  }
  study.d <- apply(study.d, 2, as.character)
  study.d<-as.data.frame(study.d)
  

    #Set "" and other missing codes to missing
  for(i in 1:ncol(study.d)){
    study.d[,i]<-ifelse(study.d[,i]=="",NA,as.character(study.d[,i]))
  } 
  
  #seperate out dynamic variables
  study.d.varying<-study.d[,which(colnames(study.d) %in% c("SUBJID", "AGEDAYS", dynamicvars))]
  
  #fill in missing static variables
  study.d.static<-study.d[,which(colnames(study.d) %in% c("SUBJID", "AGEDAYS", staticvars))]
  
  study.d.static<-study.d.static %>%  
    group_by(SUBJID) %>%
    do(fill(.,everything())) %>% 
    do(fill(.,everything(), .direction = 'up')) 

  study.d <- merge(study.d.static, study.d.varying, by=c("SUBJID","AGEDAYS"))
 
  study.d$AGEDAYS<-as.numeric(as.character(study.d$AGEDAYS))
  study.d$HAZ<-as.numeric(as.character(study.d$HAZ))
  study.d$WHZ<-as.numeric(as.character(study.d$WHZ))
  study.d$SUBJID<-as.numeric(as.character(study.d$SUBJID))

  study.d <-study.d %>% group_by(SUBJID) %>% 
    slice(which.min(abs(AGEDAYS - 30*12))) %>% #Find observation with age closest to 2 years
    filter(AGEDAYS > 30*6 & AGEDAYS < 30*18) %>% #Select ages between 1 and 3 years
    ungroup
  
  d<- bind_rows(d, study.d , .id = "studyid")
  rm(study.d)
  return(d)
}









vars<-c("STUDYID","SUBJID", "shortid",
        "SITEID","SEXN",
        "SEX","AGEDAYS",
        "CTRYCD","COUNTRY",
        "WTKG","WAZ",
        "HAZ","BAZ",
        "BMI","WHZ",
        "LENCM","LATITUDE",
        "LONGITUD","HTCM",
        "SUBJIDO","CITYTOWN",
        "ELEVATN","BIRTHWT",
        "MUACCM","MAGE",
        "MUAZ","MEDUCYRS",
        "HCIRCM","GAGEBRTH",
        "ARMCD","ARM",
        "SANITATN","NPERSON",
        "MHTCM",
        "BIRTHLEN","FEEDINGN",
        "FEEDING","TV",
        "BRTHYR","REGCTRY",
        "RADIO",
        "SUMEP","SUMDIAR",
        "SUMDAYS","PCTDIAR",
        "DLVLOC",
        "BRTHWEEK","FLOOR",
        "DELIVERY",
        "AGEDTH","FEDUCYRS",
        "PARITY","NROOMS",
        "CAR","COOKFUEL",
        "WALL",
        "VISITNUM","VISIT",
        "BICYCLE","MWORK",
        "H2OSRCP","ELEC",
        "FRIG","GAGEDAYS",
        "DEAD","NCHLDLT5",
        "ROOF",
        "MMARITN","MMARIT",
        "H2OTRTP","MWTKG",
        "MCYCLE","INCTOT",
        "APGAR5","FWORK",
        "H2OSRC","GOAT",
        "BIRTHHC","DURBRST",
        "MBMI","SEWING",
        "NCHLD","FAGE",
        "NLCHILD","PHONE",
        "COW",
        "APGAR1",
        "SMOKED","AGLAND",
        "COUGHFL","CHCIRCM",
        "LLPHONE","BED",
        "CLTHCAB","COOKPLAC",
        "NADULT","FHTCM",
        "CHICKEN","VISITDY",
        "SESN","SES",
        "SHEEP","MULTBRTH",
        "WATCH","CHAIR",
        "TABLE","STRATUMN",
        "STRATUM","EDUCCRGV",
        "RACE",
        "APGAR10","M_WTKG",
        "BFEDFL","DOGS",
        "CATTLE","H2OSRCC",
        "MRACE","METHNIC",
        "OWNHOME",
        "FAN",
        "ANIMALS","CART",
        "WASHMAC","SMOKAMT",
        "NFCHILD","NMCHILD",
        "H2OSRCB",
        "ETHNIC","M_HTCM",
        "FWTKG","FHOUSEH",
        "CATS",
        "AGLANDSZ",
        "FOODDFCT",
        "NUMHCVIS",
        "PREGOUT","RICKSHAW",
        "MATTRESS","H2OAVAIL",
        "ANTPTNUM",
        "ANTPT","MAGE1ST",
        "NSLEEP","KITCHDSC",
        "M_BMI",
        "PREECLMP",
        "SOAP","EDUCHH",
        "MSMKSTAT",
        "RODENTS",
        "VITD",
        "H2OSRCS",
        "FOWL","STOVE",
        "MHOUSEH","H2OFREQ",
        "H2OUNTRT","WASHCCHD",
        "WASHCOOK","WASHNURS",
        "R_MUAZ",
        "BEDNET","H2OSRCK",
        "FRACE",
        "SINK","SOFA",
        "NBEDROOM",
        "FSMKSTAT",
        "COHORTN","COHORT",
        "EDUCFAM",
        "INCMOM","PIGS",
        "CASTE",
        "MBRTHWT",
        "MTRIBE","FTRIBE",
        "WTKGM","HTCMM",
        "BMIM","WAZM",
        "HAZM","WHZM",
        "BAZM",
        "SMOKSTAT","SMOKYRS",
        "DIARFL",
        #WBK variables
        "STUDYID", "SUBJID",  "CLUSTID", "SITEID",  "HHID",    "SEXN",    "SEX",     "ARMCD",   "ARM",     "STRATUMN","STRATUM",
        "COHORTN", "COHORT",  "BRTHYR",  "BRTHWEEK","MHTCM",   "MEDUCYRS","CTRYCD",  "COUNTRY", "BICYCLE", "CAR",     "CHICKEN",
        "COW",     "DOGS",    "ELEC",    "GOAT",    "MOBILE",  "MCYCLE",  "HUNGER",  "RADIO",   "STOVE",   "TV",      "H2OTIME",
        "NCOMP",   "WATCH",   "FLOOR",   "COOKFUEL","ROOF",    "SANITATN","WALL",    "CHLDLT18","AGEDAYS", "VISITNUM","VISIT",  
        "WTKG",    "LENCM",   "BMI",     "HCIRCM",  "WAZ",     "HAZ",     "WHZ",     "BAZ",     "HCAZ",    "FREECHL", "IMPRLAT",
        "WATSOAP", "LNSN",    "LNSP",    "FCSAFDIS",
        #additional diarrhea vars
        "DIARFL","SUMDIAR",  
        "DIARDAYS", "CSUMDIAR",
        "DIAREPS" , "DIARBFEN" ,
        "DIARRHOEA","DIARRHOEA_NEONATAL")


dynvars<-c(
        "AGEDAYS",
        "WTKG","WAZ",
        "HAZ","BAZ",
        "BMI","WHZ",
        "LENCM","HTCM",
        "HCIRCM",
        "SUMEP","SUMDIAR",
        "SUMDAYS","PCTDIAR",
        "VISITNUM","VISIT",
        "DEAD","DURBRST",
        "COUGHFL","CHCIRCM",
       "VISITDY",
        "ANTPTNUM",
        "FREECHL", "IMPRLAT",
        "WATSOAP", "LNSN",    "LNSP",    "FCSAFDIS",
        #additional diarrhea vars
        "DIARFL","SUMDIAR",  
        "DIARDAYS", "CSUMDIAR",
        "DIAREPS" , "DIARBFEN" ,
        "DIARRHOEA","DIARRHOEA_NEONATAL")



d<-NULL 
d<-bindGHAP(study="akup", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="bfzn", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="cmc", varlist=vars, dynamicvars=dynvars, d=d)  
d<-bindGHAP(study="cmin", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="cort", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="cntt", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="ee", varlist=vars, dynamicvars=dynvars, d=d)   
d<-bindGHAP(study="eu", varlist=vars, dynamicvars=dynvars, d=d)   
d<-bindGHAP(study="gmsn", varlist=vars, dynamicvars=dynvars, d=d)
d<-bindGHAP(study="gbsc", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="irc", varlist=vars, dynamicvars=dynvars, d=d)  
d<-bindGHAP(study="jvt3", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="jvt4", varlist=vars, dynamicvars=dynvars, d=d)
d<-bindGHAP(study="knba", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="lcn5", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="mled", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="nbrt", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="ncry", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="prbt", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="phua", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="rspk", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="cmpf", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="fspp", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="tdc", varlist=vars, dynamicvars=dynvars, d=d)  
d<-bindGHAP(study="tzc2", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="zvit", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="zmrt", varlist=vars, dynamicvars=dynvars, d=d) 
d<-bindGHAP(study="lnsz", varlist=vars, dynamicvars=dynvars, d=d)
d<-bindGHAP(study="wsb", varlist=vars, dynamicvars=dynvars, d=d)
d<-bindGHAP(study="wsk", varlist=vars, dynamicvars=dynvars, d=d)
d<-bindGHAP(study="ilnd", varlist=vars, dynamicvars=dynvars, d=d)
d<-bindGHAP(study="ildm", varlist=vars, dynamicvars=dynvars, d=d)


setwd("U:/results/Stunting")
save(d, file="st_GHAPstudies_raw.Rdata")

