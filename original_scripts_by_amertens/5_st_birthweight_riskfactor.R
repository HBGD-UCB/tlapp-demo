#-----------------------------------------------------
# Birthweight risk factor for stunting and HAZ 
# in children ~2yrs old
#-----------------------------------------------------


rm(list=ls())
library(SuperLearner)
library(caret)
library(tidyverse)
library(tmle)
library(washb)

#Risk factor functions
setwd("U:/Perminant files/R scripts/Stunting analysis")
source("st_HBGDki_functions.R")
setwd("U:/results/Stunting")
load("st_GHAPstudies.Rdata")

dfull <- d

#-----------------------------------------------------
# Birthweight -unadjusted
#-----------------------------------------------------

#drop imputed birthweight
d <- filter(d, miss_BIRTHWT==0)

d$BIRTHWT <- as.numeric(d$BIRTHWT)

overall_dist_df <- d %>% filter(!is.na(BIRTHWT)) %>% 
  group_by(STUDYID, COUNTRY,SUBJID) %>% 
  slice(1) %>% ungroup() %>% 
  subset(., select=c(BIRTHWT)) %>% 
  as.data.frame()

overall_dist=quantile(overall_dist_df[,1], probs = c(.25,.5,.75), na.rm=T)

Alevels<-c(paste0("<=",round(overall_dist[1],3)), 
           paste0(round(overall_dist[1],3),"-",round(overall_dist[2],3)),
           paste0(round(overall_dist[2],3),"-",round(overall_dist[3],3)), 
           paste0(">",round(overall_dist[3],3)))


d <- d %>% filter(!is.na(BIRTHWT)) 
d<-droplevels(d)
table(d$STUDYID)

#create null W so function works with unadjusted estimation
d$w1 <- d$w2 <- rep(1, nrow(d))



HAZ_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=., 
                                   Y="HAZ", 
                                   W=c("w1","w2"), 
                                   n.cat=4, 
                                   A="BIRTHWT", 
                                   Acuts=overall_dist, 
                                   Alevels=Alevels, 
                                   reflevel=3, 
                                   family="gaussian", 
                                   SLlibrary="SL.glm", 
                                   outputdf=NULL,
                                   overall.dist=T,
                                   sparseN=5,
                                   adjusted=F)))) %>% as.data.frame()
HAZ_unadj


stunt_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=., 
                                   Y="stunt", 
                                   W=c("w1","w2"), 
                                   n.cat=4, 
                                   A="BIRTHWT", 
                                   Acuts=overall_dist, 
                                   Alevels=Alevels, 
                                   reflevel=3, 
                                   family="binomial", 
                                   SLlibrary="SL.glm", 
                                   outputdf=NULL,
                                   overall.dist=T,
                                   sparseN=5,
                                   adjusted=F)))) %>% as.data.frame()
stunt_unadj





#-----------------------------------------------------
# Birthweight -adjusted
#-----------------------------------------------------
colnames(d)

#Need to write code to limit W to # cases /20 for binomial outcomes

SLlib <- c("SL.mean","SL.glm","SL.glmnet","SL.bayesglm","SL.gam","SL.randomForest","SL.xgboost")

#faster library
SLlib <- c("SL.mean","SL.glm","SL.glmnet")


notW <- c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt","BIRTHWT","miss_BIRTHWT","region" ,"w2","w1")

Wvars <- setdiff(colnames(d),notW)

system.time(
HAZ_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=., 
                                   Y="HAZ", 
                                   W=Wvars, 
                                   n.cat=4, 
                                   A="BIRTHWT", 
                                   Acuts=overall_dist, 
                                   Alevels=Alevels, 
                                   reflevel=3, 
                                   family="gaussian", 
                                   SLlibrary=SLlib, 
                                   outputdf=NULL,
                                   overall.dist=T,
                                   sparseN=5,
                                   adjusted=T)))) %>% as.data.frame()
)
HAZ_adj

stunt_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=., 
                                   Y="stunt", 
                                   W=Wvars, 
                                   n.cat=4, 
                                   A="BIRTHWT", 
                                   Acuts=overall_dist, 
                                   Alevels=Alevels, 
                                   reflevel=3, 
                                   family="binomial", 
                                   SLlibrary=SLlib, 
                                   outputdf=NULL,
                                   overall.dist=T,
                                   sparseN=5,
                                   adjusted=T)))) %>% as.data.frame()
stunt_adj




setwd("U:/results/Stunting")
save(HAZ_unadj, stunt_unadj, HAZ_adj, stunt_adj, file="st_BIRTHWT_IRres.Rdata")


