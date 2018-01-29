

rm(list=ls())
library(dplyr)
library(tidyr)
library(SuperLearner)
library(caret)


setwd("~/andrew_scratch")
load("st_GHAPstudies_raw.Rdata")
d<-d[,-1]
d$STUDYID<-as.factor(d$STUDYID)

#Drop unrealistic values of HAZ
dim(d)
d<-d %>% filter(HAZ>-5 & HAZ<5)
dim(d)
summary(d$HAZ)


#Calculate wasting
d$wast<-ifelse(d$WHZ< (-2), 1,0)
d$wast[is.na(d$WHZ)]<-NA

d$sevwast<-ifelse(d$WHZ< (-3), 1,0)
d$sevwast[is.na(d$WHZ)]<-NA

#Calculate stunting
d$stunt<-ifelse(d$HAZ< (-2), 1,0)
d$stunt[is.na(d$HAZ)]<-NA

d$sevstunt<-ifelse(d$HAZ< (-3), 1,0)
d$sevstunt[is.na(d$HAZ)]<-NA





#################################################
# Child characteristics
#################################################

#------------------------------------------------
# 1) Sex
#------------------------------------------------
table(d$SEX)
d$SEX[d$SEX=="male"]<-"Male"
d$SEX[d$SEX=="female"]<-"Female"
d$SEX<-as.factor(d$SEX)

#------------------------------------------------
# 2) Gestational age at birth
#------------------------------------------------

d$GAGEBRTH <- as.numeric(d$GAGEBRTH)
summary(d$GAGEBRTH)

#------------------------------------------------
# 3) Birthweight
#------------------------------------------------

d$BIRTHWT <- as.numeric(d$BIRTHWT)
d$BIRTHLEN <- as.numeric(d$BIRTHLEN)

summary(d$BIRTHWT)
summary(d$BIRTHLEN)

#Drop unrealistic values
table(d$BIRTHWT[d$BIRTHWT>3400])
d$BIRTHWT[d$BIRTHWT>7000] <- NA

table(d$BIRTHLEN[d$BIRTHLEN>51])
d$BIRTHLEN[d$BIRTHLEN>70] <- NA

#------------------------------------------------
# 4) Stunted at enrollment or birth
#------------------------------------------------

#To be completed

#enrolWHZ
#load("enrol_anthro_df.Rdata")
#colnames(d_enrolHAZ)
#d_enrolHAZ<-d_enrolHAZ %>% subset(., select=c(STUDYID, SUBJID, enrolAGE, enrolHAZ, enrolWHZ)) %>%
#  filter(!is.na(enrolWHZ) ) %>% 
#  mutate(enrolWHZ=as.numeric(enrolWHZ)) %>% 
#  filter(enrolWHZ>-5 & enrolWHZ <5)  
# unique_enrol<-d_enrolHAZ %>% distinct(STUDYID, SUBJID)
# d_enrolHAZ<-left_join(unique_enrol, d_enrolHAZ ,  by=c("STUDYID", "SUBJID"))
#dim(d_enrolHAZ)
#head(d_enrolHAZ)


#d$SUBJID<-as.numeric(d$SUBJID)
#d_enrolHAZ$SUBJID<-as.numeric(d_enrolHAZ$SUBJID)
#d<-left_join(d, d_enrolHAZ, by=c("STUDYID", "SUBJID"))


#Merge in enrollment WHZ
#load("4b_enrol_anthro_df.Rdata")
#d_enrolHAZ$SUBJID<-as.numeric(d_enrolHAZ$SUBJID)
#d_enrolHAZ$STUDYID<-as.factor(d_enrolHAZ$STUDYID)
#d<-left_join(d, d_enrolHAZ, by=c("STUDYID","SUBJID"))
#Impose that enrollment must happen >1 month before measurement
#d$enrolWHZ[d$AGEDAYS-d$enrolAGE<30]<-NA
#d$enrolHAZ[d$AGEDAYS-d$enrolAGE<30]<-NA


#------------------------------------------------
# 5) Birth order
#------------------------------------------------

d$PARITY <- as.numeric(d$PARITY)


table(d$PARITY)
table(d$STUDYID,d$PARITY)

#harmonize so 1 is firstborn for all
d <-d %>% group_by(STUDYID) %>% mutate(PARITY = if(any(min(PARITY, na.rm=T)==0))  PARITY+1 else  PARITY)


d$birthorder <- NA
d$birthorder[d$PARITY==1] <- "firstborn"
d$birthorder[d$PARITY==2] <- "secondborn"
d$birthorder[d$PARITY>2] <- "thirdborn+"
table(d$birthorder)

#Note: Where is WASH Benefits?

#------------------------------------------------
# 6) Calendar month of birth
#------------------------------------------------

#"BRTHWEEK"
d$BRTHWEEK <- as.numeric(d$BRTHWEEK)

table(d$BRTHWEEK)

d$birthmonth <- round(d$BRTHWEEK/4.25)
d$birthmonth[d$birthmonth==0]<-1
table(d$birthmonth)


#------------------------------------------------
# 7) Delivery location (hospital)
#------------------------------------------------


table(d$DLVLOC)
d$homedelivery <- ifelse(d$DLVLOC=="Home" | d$DLVLOC=="Maternity home",1,0)
d$homedelivery[is.na(d$DLVLOC)] <- NA
table(d$homedelivery)

#------------------------------------------------
# 8) Delivery method (C-section vs. vaginal)
#------------------------------------------------

d$DELIVERY <- as.factor(d$DELIVERY)

table(d$DELIVERY)

d$vagbirth <- ifelse(d$DELIVERY=="Vaginal" | 
                       d$DELIVERY=="Breech Vaginal" |
                       d$DELIVERY=="Forceps" |
                       d$DELIVERY=="Normal Vaginal" |
                       d$DELIVERY=="Normal vaginal birth" |
                       d$DELIVERY=="Normal Viginal" |
                       d$DELIVERY=="Spontaneous vaginal" |
                       d$DELIVERY=="Spontaneous Vaginal Delivery" |
                       d$DELIVERY=="Suction" |
                       d$DELIVERY=="Vacuum" |
                       d$DELIVERY=="Vacuum Extraction" ,1,0)
d$vagbirth[is.na(d$DELIVERY)] <- NA
table(d$vagbirth)


#################################################
# Maternal characteristics
#################################################

#------------------------------------------------
# 9) Height
#------------------------------------------------
#Combine different variable names for maternal height
d[is.na(d$MHTCM),"MHTCM"]<-d[is.na(d$MHTCM),"M_HTCM"]

d$MHTCM <- as.numeric(d$MHTCM)


#------------------------------------------------
# 10) Weight
#------------------------------------------------



#Combine different variable names for maternal weight and BMI
d[is.na(d$MWTKG),"MWTKG"]<-d[is.na(d$MWTKG),"M_WTKG"]
d[is.na(d$MBMI),"MBMI"]<-d[is.na(d$MBMI),"M_BMI"]
d$MWTKG <- as.numeric(d$MWTKG)
d$MBMI <- as.numeric(d$MBMI)

summary(d$MWTKG)
summary(d$MBMI)


#------------------------------------------------
# 11) Age
#------------------------------------------------

d$MAGE <- as.numeric(d$MAGE)
summary(d$MAGE)

#Figure out which study does not have age classified in years
d %>% group_by(STUDYID) %>% summarize(mn=mean(MAGE,na.rm=T)) %>% filter(!is.na(mn))

d$MAGE[d$STUDYID=="ki1119695-PROBIT"] <- d$MAGE[d$STUDYID=="ki1119695-PROBIT"]/365.25

d %>% group_by(STUDYID) %>% summarize(mn=mean(MAGE,na.rm=T)) %>% filter(!is.na(mn))


#------------------------------------------------
# 12) Education
#------------------------------------------------
d$MEDUCYRS<-as.numeric(as.character(d$MEDUCYRS))

#------------------------------------------------
# 13) Marital status
#------------------------------------------------

#FHOUSEH Father lives in household
table(d$FHOUSEH)
table(d$MMARIT)

d$single <- ifelse(d$FHOUSEH=="No, permanently elsewhere",1,0)
d$single[is.na(d$FHOUSEH)] <- NA

d$single <- ifelse(d$MMARIT=="Common law" | d$MMARIT=="Married",0,1)
d$single[is.na(d$MMARIT) & is.na(d$FHOUSEH)] <- NA
table(d$single)

#------------------------------------------------
# 14) Breastfeeding practices 
#------------------------------------------------
d$breastfeeding<-"Exclusively breastfed"
d$breastfeeding[d$FEEDING=="Mixture breast/formula fed" | d$FEEDING!=""]<-"Mixed"
d$breastfeeding[d$FEEDING=="Exclusively formula fed" | d$FEEDING=="Bottle"]<-"Exclusively formula fed"
d$breastfeeding[d$FEEDING=="Unknown"]<-"Missing"
d$breastfeeding<-factor(d$breastfeeding, levels=c("Exclusively breastfed", "Mixed", "Exclusively formula fed"))
table(d$breastfeeding)


#################################################
# Paternal characteristics
#################################################

#------------------------------------------------
# 15) Height
#------------------------------------------------

#Combine different variable names for paternal height
d$FHTCM <- as.numeric(d$FHTCM)
summary(d$FHTCM)

#------------------------------------------------
# 16) Age
#------------------------------------------------
d$FAGE <- as.numeric(d$FAGE)
summary(d$FAGE)

#Figure out which study does not have age classified in years
d %>% group_by(STUDYID) %>% summarize(mn=mean(FAGE,na.rm=T)) %>% filter(!is.na(mn))

d$FAGE[d$STUDYID=="ki1119695-PROBIT"] <- d$FAGE[d$STUDYID=="ki1119695-PROBIT"]/365.25

d %>% group_by(STUDYID) %>% summarize(mn=mean(FAGE,na.rm=T)) %>% filter(!is.na(mn))


#------------------------------------------------
# 17) Education
#------------------------------------------------
d$FEDUCYRS <- as.numeric(d$FEDUCYRS)
table(d$FEDUCYRS)

################################################# 
# Household characteristics
#################################################

#------------------------------------------------
# 18) Asset-based wealth index
#------------------------------------------------

#Merge in HHwealth -asset based PCA:
d$HHwealth_quart<-0

#load("allGHAPstudies-HHwealth.Rdata")
#colnames(d)
#d<-d%>% subset(!is.na(WHZ)) %>% mutate(WHZ=as.numeric(WHZ))
#d.HHwealth<-d%>% subset(!is.na(WHZ)) %>% mutate(WHZ=as.numeric(WHZ)) %>% subset(., select=c(STUDYID, SUBJID, HHwealth_quart))
#d.HHwealth$SUBJID<-as.numeric(d.HHwealth$SUBJID)
#merge in hhwealth
#d$SUBJID<-as.numeric(d$SUBJID)
#d<-left_join(d, d.HHwealth, by=c("STUDYID", "SUBJID"))

#SES
table(d$SES)

#income
d$INCTOT <- as.numeric(d$INCTOT)
summary(d$INCTOT)


#------------------------------------------------
# 19) Number of rooms
#------------------------------------------------

d$NROOMS <- as.numeric(d$NROOMS)
table(d$NROOMS)
d$NROOMS[d$NROOMS>10] <- 10

#------------------------------------------------
# 20) Total number of persons
#------------------------------------------------

d$NCOMP <- as.numeric(d$NCOMP)
table(d$NCOMP)

#------------------------------------------------
# 21) Number of children <5y
#------------------------------------------------

d$NCHLDLT5 <- as.numeric(d$NCHLDLT5)
table(d$NCHLDLT5)

#------------------------------------------------
# 22) Animals (owns chickens, owns cow/buffalo)
#------------------------------------------------

#chicken
d$CHICKEN[d$CHICKEN=="Y"]<-"1"
d$CHICKEN<-as.numeric(as.character(d$CHICKEN))
table(d$CHICKEN)
d$chicken<-ifelse(d$CHICKEN==0,0,1)
d$chicken[d$CHICKEN==""]<-NA
table(d$chicken)

#cattle
d$CATTLE[is.na(d$CATTLE)]<-d$COW[is.na(d$CATTLE)]
table(d$CATTLE)

d$CATTLE<-as.character(d$CATTLE)
d$CATTLE[d$CATTLE=="Yes"]<-"1"
d$CATTLE[d$CATTLE=="No"]<-"0"
d$CATTLE[d$CATTLE==" 1"]<-"1"
d$CATTLE[d$CATTLE==" 0"]<-"0"
d$CATTLE[d$CATTLE==""]<-"NA"
d$CATTLE<-as.numeric(as.character(d$CATTLE))

d$cow<-ifelse(d$CATTLE>0,1,0)
d$cow[is.na(d$CATTLE)]<-NA
table(d$cow)


#------------------------------------------------
# 23) Improved floor (vs. unimproved)
#------------------------------------------------

table(d$FLOOR)
d$improved.floor<-ifelse(d$FLOOR=="Clay" | d$FLOOR=="Clay and Dung and Clay" | d$FLOOR=="Clay and Sand" | d$FLOOR=="Dirt as floor" | d$FLOOR=="Dung" |
                           d$FLOOR=="Dung and Clay" | d$FLOOR=="Dung and Clay and Sand" | d$FLOOR=="earth" | d$FLOOR=="Earth" | d$FLOOR=="Earth or bamboo" |
                           d$FLOOR=="Earth/Sand" | d$FLOOR=="ground" | d$FLOOR=="Ground" | d$FLOOR=="HALF SEND AND HALF CEMMENTED." | d$FLOOR=="HOUSE BEING BUILD" |
                           d$FLOOR=="Katcha" | d$FLOOR=="Katcha/Mud" | d$FLOOR=="MUD,GRAVEL" | d$FLOOR=="ONE ROOM HAS CEMENT AND ONE SAND (FLOOR)" |
                           d$FLOOR=="Palm/Bamboo" | d$FLOOR=="Thatch, grass, sticks, branches" | d$FLOOR=="cane" | d$FLOOR=="0"
                         ,0,1)

d$improved.floor[d$FLOOR == "F"  | d$FLOOR == "Does not know" | d$FLOOR == "No walls/ Fence" | d$FLOOR=="Other" | d$FLOOR=="other" | d$FLOOR==""]<-NA


#------------------------------------------------
# 24) Sanitation facility (JMP definitions)
#------------------------------------------------
#Replace sanitation with sanitation arm from WASH Benefits studies
table(d$ARM)

d$SANITATN[d$shortid=="wsb" | d$shortid=="wsk"]<-"No sanitation intervention"
d$SANITATN[d$ARM=="WSH" | d$ARM=="Sanitation" | d$ARM=="Nutrition + WSH"]<-"Sanitation intervention"
table(d$SANITATN)


d$improved.sanitation<-ifelse(
  d$SANITATN=="Drain connected inside house" |
    d$SANITATN=="Flush to piped sewer system" |
    d$SANITATN=="Flush to septic tank." |
    d$SANITATN=="Flush toilet" |
    d$SANITATN=="Inside latrine with drainage" |
    d$SANITATN=="Latrine with flush system" |
    d$SANITATN=="Own flush latrine" |
    d$SANITATN=="Septic tank or toilet" |
    d$SANITATN=="Latrine Septic tank/ Modern toilet" |
    d$SANITATN=="Water Closet" |
    d$SANITATN=="1" |    d$SANITATN==" 1" |
    d$SANITATN=="concrete slb" |
    d$SANITATN=="concrete slb|potty" |
    d$SANITATN=="concrete slb|waterseal" |
    d$SANITATN=="concrete slb|waterseal|potty" |
    d$SANITATN=="Own latrine|concrete slb" |
    d$SANITATN=="Own latrine|concrete slb|potty" |
    d$SANITATN=="Own latrine|concrete slb|waterseal" |
    d$SANITATN=="Own latrine|concrete slb|waterseal|potty" |
    d$SANITATN=="Own latrine|waterseal" |
    d$SANITATN=="Pacca latrine (water seal)" |
    d$SANITATN=="Sanitary" |
    d$SANITATN=="VIP Latrine w/ Water Seal" |
    d$SANITATN=="Water-sealed or slab latrine" |
    d$SANITATN=="Water sealed/slab" |
    d$SANITATN=="waterseal" |
    d$SANITATN=="Y"|
    d$SANITATN=="Sanitation intervention"|
    d$SANITATN=="toilet" |
    d$SANITATN=="TOILET" |
    d$SANITATN=="Vent. Impr. pit latrine" |
    d$SANITATN=="Vent.impr.pit latrine" |
    d$SANITATN=="VIP Latrine" |
    d$SANITATN=="Pour Flush Toilet",
  1,0)

d$improved.sanitation[d$SANITATN=="" | d$SANITATN=="9" | d$SANITATN==" 9" | d$SANITATN=="Other" | d$SANITATN=="other"]<-NA

#Merge in improved latrine indicator from WASH Benefits
#"IMPRLAT"


#------------------------------------------------
# 25) Safe water source (JMP definitions)
#------------------------------------------------

#Replace water source with water arm from WASH Benefits studies
table(d$ARM)
d$H2OSRC[d$shortid=="wsb" | d$shortid=="wsk"]<-"No water intervention"
d$H2OSRC[d$ARM=="WSH" | d$ARM=="Water" | d$ARM=="Nutrition + WSH"]<-"Safe water intervention"
table(d$H2OSRC)


table(d$H2OSRC)
d$safe.water<-ifelse(
  d$H2OSRC %in%
    c("Safe water intervention",
      "Deep Bore Hole and Public Tap",
      "Public Handpump and Public Tap",
      "Private Handpump and Public Tap", 
      "Pipe and Public Tap",
      "Deep Bore Hole and Public Tap",
      "Private Well",
      "Public Handpump and Public Tap",
      "Public Tap",
      "Cover well in neigbours",
      "Bore hole tap water into neighbor's comp",
      "Covered well from neighbor's comp", 
      "Tap water into neighbor's comp",
      "Tap water from neighbor's comp",   
      "Hand pump",                 
      "Public tap",
      "Taken from mosque next door through pipe",
      "Cover well in neigbours", 
      "Bore hole tap water into neighbor's comp",
      "Covered well from neighbor's comp",
      "Tap water into neighbor's comp",
      "Tap water from neighbor's comp",
      "Covered well into neighbours camp",
      "Tap in neighbor's compound",
      "Pipe into neighbor's yard",
      "Pipe into neighbor hosue", 
      "Tap in neighbours compound",
      "Piped into relative house",
      "Borrowed from neighbour through pipe",
      "From waterpump",                         
      "Piped from neighbors",   
      "Land lord's tap",  
      "Rainwater",
      "Piped water at the neighbors",           
      "Bore hole at neighbors",  
      "Protected spring",
      "Tap into a neighbors underground", 
      "Tap service office",
      "Covered private well",
      "Tap in house",                            "Connection in Home for 24hrs",           
      "Other Source",                           
      "Private tap",                             "Community tap",                          
      "Private tubewell/ hand pump",             "Community tubewell",                     
      "Piped into dwelling from treated faclty",
      "Piped into dwelling potable",             "Piped to compnd, yard or plot-trt faclty",
      "Piped to compnd, yard or plot-potable",                         "Piped to neighbor -potable",             
      "public tap-treatment faclty",             "dug well- protected",                    
      "municipal network",                                                       
      "Deep tube well",                         
      "Piped into yard",                        
      "Bore hole",                               "Covered public well",                    
      "Piped into neighbors compound",          
      "Piped into house",                                 
      "Piped into nearby compound",              "Covered well in house or yard")
  ,1,0)
table(d$safe.water)


d$safe.water[d$H2OSRC=="Other" | d$H2OSRC=="other" | d$H2OSRC=="Other Source" | d$H2OSRC==""
             | d$H2OSRC=="Self-made waterline is near: N/A" | d$H2OSRC=="Self-made waterline is near: Drain" | 
               d$H2OSRC=="Self-made waterline is near: Latrine" | d$H2OSRC=="Drinking water" ]<-NA
table(d$safe.water)

#------------------------------------------------
# 26) Treats drinking water
#------------------------------------------------

#Replace water treatment with water arm from WASH Benefits studies
table(d$ARM)
d$H2OTRTP[d$shortid=="wsb" | d$shortid=="wsk"]<-"No water intervention"
d$H2OTRTP[d$ARM=="WSH" | d$ARM=="Water" | d$ARM=="Nutrition + WSH"]<-"Safe water intervention"
table(d$H2OTRTP)

d$treat.water<-ifelse(
  d$H2OTRTP=="Strain Through Cloth" | 
    d$H2OTRTP=="No water intervention" |
    d$H2OTRTP=="Soda" | 
    d$H2OTRTP=="None" | 
    d$H2OTRTP=="Leave water in sun" | 
    d$H2OTRTP=="Let It Stand" | 
    d$H2OTRTP=="Filter through a cloth" | 
    d$H2OTRTP=="Do not treat" | 
    d$H2OTRTP=="Not Boiled" | 
    d$H2OTRTP=="No treatment" | 
    d$H2OTRTP=="Alum" | 
    d$H2OTRTP=="Own Arrangement by plastic pipe" | 
    d$H2OTRTP=="Municipality supply" | 
    d$H2OTRTP=="Alum" 
  ,0,1)

table(d$treat.water)

d$treat.water[d$H2OTRTP=="does not know" | d$H2OTRTP=="unknown or not applicable" | d$H2OTRTP=="Other"  | d$H2OTRTP=="Others"| d$H2OTRTP==""]<-NA



#Merge in the unsafe water variable
table(d$H2OUNTRT)


#------------------------------------------------
# 27) Soap present in the home
#------------------------------------------------

table(d$SOAP)

#Add in uptake indicator from WASH B
#"WATSOAP"

#------------------------------------------------
# 28) Cooking fuel usage
#------------------------------------------------

table(d$COOKFUEL)

d$cleancook <- ifelse(d$COOKFUEL=="Electricity"|
                        d$COOKFUEL=="Gas"|
                        d$COOKFUEL=="Gas cylinder stove"|
                        d$COOKFUEL=="gas stove"|
                        d$COOKFUEL=="Gas Stove"|
                        d$COOKFUEL=="Kerosene oil stove"|
                        d$COOKFUEL=="kerosene stove"|
                        d$COOKFUEL=="more than one",1,0)
d$cleancook[is.na(d$COOKFUEL)] <- NA
d$cleancook[d$COOKFUEL=="Other" | d$COOKFUEL=="Garments products"] <- NA
table(d$cleancook)


#------------------------------------------------
# 28.5) Food security
#------------------------------------------------

table(d$FOODDFCT)



#################################################
# Time-varying characteristics
#################################################

#------------------------------------------------
# 29) Current diarrhea
#------------------------------------------------
d$DIARFL[d$DIARFL==" 0"]<-"0"
d$DIARFL[d$DIARFL==" 1"]<-"1"
d$DIARFL<-as.numeric(d$DIARFL)

#------------------------------------------------
# 30) Cumulative days of diarrhea
#------------------------------------------------
#create harmonized diarrhea variable -add in maled
table(d$STUDYID, is.na(d$PCTDIAR))
d$PCTDIAR[d$STUDYID=="MAL-ED"]<-as.numeric(as.character(d$DIARDAYS[d$STUDYID=="MAL-ED"]))/30*100

#Other variables
#"SUMEP","SUMDIAR",
#"SUMDAYS",

#"DIARFL","SUMDIAR",  
#"DIARDAYS", "CSUMDIAR",
#"DIAREPS" , "DIARBFEN" ,
#"DIARRHOEA","DIARRHOEA_NEONATAL"

#------------------------------------------------
# 31) Breastfeeding duration
#------------------------------------------------

#"DURBRST"
table(d$DURBRST)
d$DURBRST <- as.numeric(d$DURBRST)

#------------------------------------------------
# 32) Season (Month of measurement)
#------------------------------------------------

#"BRTHWEEK"

d$month <- floor(d$BRTHWEEK/4 + d$AGEDAYS/30.25)
table(d$month)
d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
d$month[d$month>12 & !is.na(d$month)] <- d$month[d$month>12 & !is.na(d$month)]-12 
table(d$month)


#################################################
# Intervention ARM
#################################################


#Replace nondescript intervention names
d$ARM[d$STUDYID=="ki1000125-AgaKhanUniv" & d$ARM=="Intervention"] <-"Education"
d$ARM[d$STUDYID=="ki1000304b-SAS-CompFeed" & d$ARM=="Intervention"] <-"Comp. feeding education"


d$tr <- NA
d$tr[d$ARM=="Control" | d$ARM=="Control (no Zinc)" | d$ARM=="Standard(Control)" | d$ARM=="No intervention" | d$ARM=="Placebo" | d$ARM=="Passive Control" | d$ARM=="no zinc, no copper" 
     | d$ARM=="Iron and folic acid supplementation" | d$ARM=="e.Control" | d$ARM=="Likuni Phala" | d$ARM=="WPC" | d$ARM== "CFC"  | d$ARM=="Placebo nippled + Placebo Oval" | d$ARM=="Iron Folic Acid"] <- "C"

d$tr[d$ARM=="Therapeutic Zinc: 20 mg/day for 10 days" | d$ARM=="3 mg zinc, no copper" | d$ARM=="10 mg zinc, no copper" |  d$ARM=="10 mg zinc, with copper" |  d$ARM=="3 mg zinc, no copper" | 
       d$ARM=="Intermittent Zinc: 10 mg/d for 10 days" | d$ARM=="Preventive Zinc: 7 mg/day" | d$ARM=="Zinc Alone" | d$ARM=="7 mg zinc, no copper" |d$ARM=="Zinc" ] <- "Zinc"

d$tr[d$ARM=="a.LNS-Zn0" | d$ARM=="b.LNS-Zn5" | d$ARM=="c.LNS-Zn10" | d$ARM=="d.LNS-TabZn5" | d$ARM=="LNS-20gNoM" | d$ARM=="LNS-20gM" | d$ARM=="LNS-10gM" | d$ARM=="LNS-40gM" | d$ARM=="LNS-40gNoM" | d$ARM=="Nutrition" | d$ARM=="Nutrition + WSH" | d$ARM=="Lipid-based nutrient supplementation"  |  d$ARM=="Plumpy Doz" |  d$ARM=="Milk FS" | d$ARM=="Soy FS"] <- "LNS"

d$tr[d$ARM=="WSH" | d$ARM=="Water" | d$ARM=="Handwashing" | d$ARM=="Sanitation" | d$ARM=="Education" | d$ARM=="Visitation" | d$ARM=="Nutritional counselling" | d$ARM=="Vitamin D"  | 
       d$ARM=="50,000 IU nippled + 400,000 IU Oval" | d$ARM=="Placebo nippled + 400,000 IU Oval" | d$ARM=="50,000 IU nippled + Placebo Oval" | d$ARM=="BSC" | d$ARM=="Comp. feeding education" | d$ARM=="Nutritional counselling" | d$ARM=="Visitation"] <- "Other"

d$tr[d$ARM=="Multiple micronutrient supplementation" | d$ARM=="Multivitamin Alone" | d$ARM=="Zinc + Multivitamin" |  d$ARM=="MNT + WPC" | d$ARM=="MNT + BSC" | d$ARM=="Multiple Micronutrients" ] <- "MMN"

d$tr[d$ARM=="Food supplementation" | d$ARM=="Chickpea" | d$ARM=="Rice Lentil" | d$ARM=="WSB++" ] <- "CF"

d$tr <- factor(d$tr)

table(is.na(d$tr))
d$ARM[is.na(d$tr)]
d$STUDYID[is.na(d$tr)]

table(d$STUDYID, d$tr)




#################################################
# Others not yet registered
#################################################


#Bednet
#d$bednet<-ifelse(d$BEDNET==" 0",0,1)
#d$bednet[is.na(d$BEDNET)]<-NA







#Create a region of the world variable
unique(d$COUNTRY)
d$region<-"Missing"
d$region[d$COUNTRY=="BURKINA FASO" | d$COUNTRY=="GAMBIA" | d$COUNTRY=="GUINEA-BISSAU" 
         | d$COUNTRY=="KENYA" | d$COUNTRY=="MALAWI" | d$COUNTRY=="MALI" | d$COUNTRY=="MOZAMBIQUE"
         | d$COUNTRY=="SOUTH AFRICA" | d$COUNTRY=="TANZANIA, UNITED REPUBLIC OF" | d$COUNTRY=="ZIMBABWE"]<-"SSA"
d$region[d$COUNTRY=="BANGLADESH" | d$COUNTRY=="INDIA" | d$COUNTRY=="NEPAL" | d$COUNTRY=="PAKISTAN" ]<-"south Asia"
d$region[d$COUNTRY=="CHINA" | d$COUNTRY=="PHILIPPINES" | d$COUNTRY=="NEPAL" | d$COUNTRY=="PAKISTAN" ]<-"east Asia"
d$region[d$COUNTRY=="BRAZIL" | d$COUNTRY=="ECUADOR" | d$COUNTRY=="GUATEMALA" | d$COUNTRY=="PERU" ]<-"Latin America"
d$region[d$COUNTRY=="BELARUS"]<-"Europe"
table(d$region)
d$region<-as.factor(d$region)
d$COUNTRY<-as.factor(d$COUNTRY)








d<- d %>% subset(., select=c(
  STUDYID,
  COUNTRY,
  SUBJID,
  AGEDAYS,
  HAZ,
  stunt,
  sevstunt,
  SEX,
  GAGEBRTH,
  BIRTHLEN,
  BIRTHWT,
  birthorder,
  birthmonth,
  homedelivery,
  vagbirth,
  MHTCM,
  MWTKG, 
  MBMI, 
  MAGE, 
  MEDUCYRS,
  single,
  breastfeeding,
  FHTCM, 
  FAGE,
  FEDUCYRS,
  SES,
  NROOMS,
  NCOMP,
  NCHLDLT5,
  chicken,
  cow,
  improved.floor,
  improved.sanitation,
  safe.water,
  treat.water,
  SOAP,
  cleancook,
  DIARFL,
  DURBRST, 
  month,
  tr, 
  region)) %>% filter(HAZ>-5 & HAZ <5)

# Impute missing data and make flagged indicators for missing values




d$birthorder <- factor(d$birthorder)
d$SES <- factor(d$SES)
d$SOAP <- factor(d$SOAP)

save(d, file="st_GHAPstudies.Rdata")

