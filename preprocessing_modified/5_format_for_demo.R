library(tidyverse)

load("~/andrew_scratch/st_GHAPstudies.Rdata")

dfull <- d

#-----------------------------------------------------
# Birthweight -unadjusted
#-----------------------------------------------------

d$BIRTHWT <- as.numeric(d$BIRTHWT)
d <- d %>% filter(!is.na(BIRTHWT)) 
d<-droplevels(d)

write.csv(d, "~/UCB-SuperLearner/tlapp-demo/demo_data.csv", row.names=FALSE)

subset <- d[d$STUDYID=="ki1148112-iLiNS-DYAD-M",]

write.csv(subset, "~/UCB-SuperLearner/tlapp-demo/demo_data_iLiNS_subset.csv", row.names=FALSE)
