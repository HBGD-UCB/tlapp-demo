rm(list=ls())
library(ghap)
library(dplyr)

set_git_base_path("~/git")
get_git_base_path()


setwd("~/andrew_scratch")








d<-use_study("agakhanuniv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="akup.rds") 

d<-use_study("burkina_faso_zn")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bfzn.rds") 

d<-use_study("cmc_v_bcs_2002") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmc.rds")  

d<-use_study("cmin")       
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmin.rds") 

d<-use_study("cohorts")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cort.rds") 

d<-use_study("content")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cntt.rds") 

d<-use_study("ee")             
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ee.rds")   

d<-use_study("eu") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eu.rds")   

d<-use_study("gms_nepal")      
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gmsn.rds")

d<-use_study("gusto")          
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gsto.rds") 

d<-use_study("guatemala_bsc")  
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gbsc.rds") 

d<-use_study("irc")            
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="irc.rds")  

d<-use_study("jivita_3")       
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="jvt3.rds") 

d<-use_study("jivita_4")       
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="jvt4.rds")

d<-use_study("keneba")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="knba.rds") 

d<-use_study("lcni_5")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="lcn5.rds") 

d<-use_study("mal_ed")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mled.rds") 

d<-use_study("nih_birth")      
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="nbrt.rds") 

d<-use_study("probit")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="prbt.rds") 

d<-use_study("peru_huascar")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="phua.rds") 

d<-use_study("respak")         
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="rspk.rds") 

d<-use_study("sas_compfeed")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmpf.rds") 

d<-use_study("sas_foodsuppl")  
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="fspp.rds") 

d<-use_study("tdc")            
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="tdc.rds")  

d<-use_study("tanzaniachild2") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="tzc2.rds") 

d<-use_study("zvitambo")      
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zvit.rds") 

d<-use_study("znmort")        
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zmrt.rds") 

d<-use_study("ilins_zinc") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="lnsz.rds")



d<-use_study("wash_bangladesh") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="wsb.rds")


d<-use_study("wash_kenya") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="wsk.rds")


d<-use_study("bigcs_ultrasound") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bigu.rds")

d<-use_study("ilins_dose") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ilnd.rds")

d<-use_study("ilins_dyad_m") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ildm.rds")

d<-use_study("imnci") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="imnc.rds")

d<-use_study("amanhi") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="amni.rds")

d<-use_study("peru_zn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="pzn.rds")

d<-use_study("Ecuador Egg") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eegg.rds")

d<-use_study("Bangladesh Diarrhea") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bngd.rds")





#Add studies neeeded to be added to metadata
d<-use_study("ncry") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ncry.rds")


d<-use_study("zinf") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zinf.rds")


d<-use_study("gual") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gual.rds")


d<-use_study("ppd") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ppd.rds")


d<-use_study("mahn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mahn.rds")


d<-use_study("incp") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="incp.rds")


d<-use_study("gsto") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gsto.rds")


d<-use_study("grip") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="grip.rds")


d<-use_study("gtwn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gtwn.rds")


d<-use_study("eczn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eczn.rds")

d<-use_study("prvd") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="prvd.rds")


d<-use_study("mmam") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mmam.rds")


d<-use_study("dvds") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="dvds.rds")


d<-use_study("bigu") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bigu.rds")
















