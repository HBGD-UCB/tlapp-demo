



#---------------------------------------
# RiskFactorFunction.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Function to estimate ATE with TMLE
# across levels of a risk factor
#---------------------------------------

tmle_risk<-function(dat=d, 
                    Y="HAZ", 
                    W=Wvars, 
                    n.cat=2, 
                    A, 
                    Acuts=NULL, 
                    Alevels=NULL, 
                    reflevel=NULL, 
                    family="gaussian", 
                    SLlibrary=library, 
                    outputdf=res.df,
                    overall.dist=T,
                    sparseN=5,
                    adjusted=F){
  
  #get number of studies 
  nstudies<-length(unique(dat$STUDYID))
  
  if(A %in% W){W<-W[-which(W %in% A)]}
  y<-subset(dat, select=Y)
  a<-subset(dat, select=A)
  dat$STUDYID<-as.factor(dat$STUDYID)
  studyid<-subset(dat, select="STUDYID")
  subjid<-subset(dat, select="SUBJID")
  
  if(overall.dist==F & class(a[,1])=="numeric"){
    Acuts<-as.numeric(quantile(a[,1], probs = c((1:(n.cat-1))/n.cat), na.rm=T))
    if(n.cat==2) Alevels<-c(paste0("<=",round(Acuts[1],3)),  paste0(">",round(Acuts[1],3)))
    if(n.cat==3) Alevels<-c(paste0("<=",round(Acuts[1],3)), paste0(round(Acuts[1],3),"-",round(Acuts[2],3)), paste0(">",round(Acuts[2],3)))
    if(n.cat==4) Alevels<-c(paste0("<=",round(Acuts[1],3)), paste0(round(Acuts[1],3),"-",round(Acuts[2],3)), paste0(round(Acuts[2],3),"-",round(Acuts[3],3)), paste0(">",round(Acuts[3],3)))
  }
  
  reference<-Alevels[reflevel]
  comparisons<-Alevels[-reflevel]
  
  
  summary(a[,1])
  
  table(findInterval(a[,1], Acuts, left.open=T))
  
  if(!is.null(Acuts)){
    a[,1]<-findInterval(a[,1], Acuts, left.open=T)
    a[,1]<-factor(a[,1])
    if(!is.null(Alevels)){levels(a[,1])<-Alevels[as.numeric(levels(a[,1]))+1]}
  }
  
  a[,1]<-as.factor(a[,1])
  print(table(a[,1]))
  
  w<-subset(dat, select=c(W))
    #remove sparse covariates
  if(adjusted==T){
    dim(w)
    w <- w[ , colSums(is.na(w)) == 0]
    #Drop factors with no variation
    w<-droplevels(w)
    w <- w[, sapply(w, nlevels) > 1 | sapply(w, is.numeric)]
    preproc = caret::preProcess(w, method = c("zv", "nzv"))
    w = predict(preproc, w)
    dim(w)
    print(colnames(w))
  }

  
  
  fulldat<-data.frame(y,a,studyid,subjid,w)
  fulldat<-fulldat[complete.cases(fulldat),]
  
  #Extract mean Y|A
  levelmeans<- fulldat %>% #fulldat[fulldat[,2]==levels(fulldat[,2])[1],] %>%
    group_by(.[[2]]) %>%
    do(as.data.frame(washb_mean(Y=.[[1]], id=1:length(.[[1]]), print = F))) %>% 
    as.data.frame %>% `rownames<-`(.[,1]) #%>% .[,-1]
  
  
  #Extract desired levels
  levelmeans<-levelmeans[1:n.cat,]
  

#Code for TMLE3
  
  #NOTE: Make sure I'm feeding in a factor, not numeric A, for the multinomial
  
  # nodes <- list(W=colnames(w),
  #               A=A,
  #               Y=Y)
  # 
  # lrnr_glm_fast <- make_learner(Lrnr_glm_fast)
  # lrnr_mean <- make_learner(Lrnr_mean)
  # learner_list <- list(Y=lrnr_mean, A=lrnr_glm_fast)
  # 
  # d<- data.table(fulldat)
  # tmle_fit_from_spec <- tmle3(tmle_TSM_all(),d, nodes, learner_list)
  # print(tmle_fit_from_spec)
  # 
  # tmle_fit_from_spec$summary
  # tmle_fit_from_spec$delta_summary
  # 
  # 
  #   
  # tmle_fit_PAF <- tmle3(tmle_PAR(baseline_level = 0),d, nodes, learner_list)
  # print(tmle_fit_PAF)
  # tmle_fit_PAF$delta_summary

  
  res<-NULL
  for(i in comparisons){

    dat<-fulldat[fulldat[,2]==reference | fulldat[,2]==i,]
    # print(table(dat[,2], dat[,1]))

    print(table(dat[,2]))

    #Convert factor to binary indicator
    dat[,2]<-ifelse(dat[,2]==reference,0,1)

    table(dat[,1],dat[,2])
    if(family=="binomial"){print(c(table(dat[,1],dat[,2])))}
    sparse=F
    if(family=="binomial"){
      tab<-c(table(dat[,1],dat[,2]))
      if(tab[1]<sparseN+1 | tab[2]<sparseN+1  | tab[3]<sparseN+1  | tab[4]<sparseN+1 | is.na(tab[1]) | is.na(tab[2]) | is.na(tab[3]) | is.na(tab[4])){
        sparse=T
      }
    }



    if(sum(dat[,2]==0)>sparseN & sum(dat[,2]==1)>sparseN & sparse==F){ #Make sure there is enough support in the data
      w<-as.data.frame(dat[,5:ncol(dat)])
      if(ncol(w)==1){colnames(w)<-colnames(dat)[5]<-"colW"}

      if(adjusted==T){
        if(family=="binomial"){
          Wscreen <- hbgdki_prescreen(Y=dat[,1], Ws=droplevels(w), ncases=sum(dat[,1]))
          }else{
            Wscreen <- washb_prescreen(Y=dat[,1], Ws=droplevels(w))
            }
        if(length(Wscreen)>0){
        w<-subset(w, select=Wscreen)
        }else{
          dat$w1<-rep(1, nrow(dat))
          dat$w2<-rep(1, nrow(dat))
          w<-data.frame(w1=dat$w1, w2=dat$w2)
        }
        if(length(Wscreen)==1){ #add null covariate so glmnet works
        dat$w2 <- w$w2 <- rep(1, nrow(dat))
        }
      }

      fit<-studyCV.tmle(d=dat,
                        Y=Y,
                        Avar=A,
                        nstudies=nstudies,
                        lib=SLlibrary,
                        family=family,
                        Wvars=colnames(w),
                        CVstudies=ifelse(nstudies==1,F,T))

      out<-as.data.frame(fit)
      names(out)<-i
      out<-t(out)
      out<-data.frame(A,i,out,reference,compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))

      if(family=="binomial"){
        out<-data.frame(out,t(c(table(dat[,1],dat[,2]))))
        colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue","RR", "RRCI1", "RRCI2","RRpvalue","log.RR","var.log.RR", "reference", "compN", "refN","d","c","b","a")
      }else{
        colnames(out)<-c("variable","level","psi","var.psi","CI1","CI2","pvalue", "reference", "compN", "refN")
      }


    }else{
      if(family=="binomial"){
        out<-data.frame(variable=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, RR=NA,  RRCI1=NA, RRCI2=NA, RRpvalue=NA, log.RR=NA, var.log.RR=NA, reference=reference, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1), d=tab[1], c=tab[2], b=tab[3], a=tab[4])

      }else{
        out<-data.frame(variable=A,level=i,psi=NA, var.psi=NA, CI1=NA, CI2=NA, pvalue=NA, reference=reference, compN=sum(dat[,2]==0), refN=sum(dat[,2]==1))
      }
      rownames(out)<-i
    }
    res<-rbind(res,out)
  }

  
  if(family=="binomial"){
    refrow<-data.frame(res[1,1],reference,t(rep(NA,11)),reference,t(rep(NA,6)))
  }else{
    refrow<-data.frame(res[1,1],reference,t(rep(NA,5)),reference,t(rep(NA,2)))
  }
  colnames(refrow)<-colnames(res)
  res<-rbind(refrow,res)
  
  res<-cbind(res,levelmeans)
  
  if(family=="binomial"){
    colnames(res)<-c("variable","level","ATE","ATE.var","ATE.CI1","ATE.CI2", "ATE.Pval","RR","RR.CI1","RR.CI2", "RR.Pval", "logRR.psi","logRR.var","reference", "compN", "refN","a","b","c","d", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }else{
    colnames(res)<-c("variable","level","ATE","var","CI1","CI2", "Pval","reference", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }
  rownames(res)<-NULL

  
  if(!is.null(outputdf)){
    return(rbind(outputdf,res))
  }else{
    #return(list(res=res, PAFdat=PAFdat))
    return(res)
  }
}






studyCV.tmle<-function(d, 
                       Y,
                       Avar, 
                       nstudies,
                       lib=lib, 
                       Wvars,
                       family,
                       CVstudies=T){
  
  set.seed(12345)
  d<-as.data.frame(d)
  Y<-subset(d, select=Y)
  subjid<-subset(d, select="SUBJID")
  W<-subset(d, select=Wvars)
  W<-design_matrix(as.data.frame(W))
  
  if(nstudies>1){
    
    mixed.CVfolds<-createFolds(factor(d$STUDYID), k = 5, list = TRUE)
    
    
    X<-cbind(d[,Avar], W)
    colnames(X)[1]<-Avar
    Qsl<-SuperLearner(Y=Y[,1],
                      #X=cbind(d[,Avar], subset(d, select=Wvars)),
                      X=X,
                      SL.library = lib,
                      family=family,
                      cvControl = ifelse(CVstudies==T,
                                         list(V=5, validRows=mixed.CVfolds),
                                         list(V=5)))
    dY1<-dY0<-as.data.frame(X)
    dY1[,Avar]<-1
    dY0[,Avar]<-0
    Q<-cbind(predict(Qsl, newdata = dY1)$pred,predict(Qsl, newdata = dY0)$pred)
    head(Q)
    
    
    gsl<-SuperLearner(Y=subset(d, select=Avar)[,1],
                      X=X[,-1],
                      SL.library = lib,
                      family=family,
                      cvControl = ifelse(CVstudies==T,
                                         list(V=5, validRows=mixed.CVfolds),
                                         list(V=5)))
    
    g1W<-predict(gsl)$pred
    
    
    
    suppressWarnings(mixedCV.tmle.A<-tmle(Y=Y[,1], 
                         A=subset(d, select=Avar)[,1], 
                         W=X[,-1], 
                         Q=Q,
                         g1W=g1W,
                         family = family, 
                         verbose = F))
    
  }else{
    suppressWarnings(mixedCV.tmle.A<-tmle(Y=Y[,1], 
                         A=subset(d, select=Avar)[,1], 
                         W=W, 
                         family = family, 
                         Q.SL.library=lib,
                         g.SL.library = lib,
                         verbose = F,
                         id=subjid[,1]))  
    
  }
  if(family=="binomial"){
    return(c(unlist(mixedCV.tmle.A$estimates$ATE),unlist(mixedCV.tmle.A$estimates$RR)))
  }else{
    return(unlist(mixedCV.tmle.A$estimates$ATE))
  }
}



#Function to facilitate binding out risk factor analysis output across dplyr groups
select_groups <- function(data, groups, ...) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]}





# --------------------------------------
# function to prescreen covariates and 
# select 1 covariate per 10 cases
# --------------------------------------

hbgdki_prescreen <- function (Y, Ws, ncases, family = "binomial", pval = 0.2,  print = TRUE){
  
   n<-nrow(Ws)
   if(n-ncases < ncases){ncases<-n-ncases}  
  
    require(lmtest)
    if(family[[1]]=="neg.binom"){
       require(MASS)
    }
    if(pval > 0.99 | pval < 0){
        stop("P-value threshold not set between 0 and 1.")
    }
    Ws <- as.data.frame(Ws)
    dat <- data.frame(Ws, Y)
    dat <- dat[complete.cases(dat), ]
    nW <- ncol(Ws)
    LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
    rownames(LRp) <- names(Ws)
    colnames(LRp) <- "P-value"
    if(family[[1]] != "neg.binom"){
        for(i in 1:nW) {
            dat$W <- dat[, i]
            if(class(dat$W) == "factor" & dim(table(dat$W)) == 
                1) {
                fit1 <- fit0 <- glm(Y ~ 1, data = dat, family = family)
            }
            else{
                fit1 <- glm(Y ~ W, data = dat, family = family)
                fit0 <- glm(Y ~ 1, data = dat, family = family)
            }
            LRp[i] <- lrtest(fit1, fit0)[2, 5]
        }
    }
    else{
        if(!requireNamespace("MASS", quietly = TRUE)){
            stop("Pkg needed forthis function to work. Please install it.", 
                call. = FALSE)
        }
        else{
            for(i in 1:nW){
                dat$W <- dat[, i]
                if(class(dat$W) == "factor" & dim(table(dat$W)) == 
                  1) {
                  fit1 <- fit0 <- glm(Y ~ 1, data = dat, family = family)
                }
                else{
                  fit1 <- glm.nb(Y ~ W, data = dat, family = family)
                  fit0 <- glm.nb(Y ~ 1, data = dat, family = family)
                }
                LRp[i] <- lrtest(fit1, fit0)[2, 5]
            }
        }
    }
    p20 <- ifelse(LRp < pval, 1, 0)
    if(print == TRUE) {
        cat("\nLikelihood Ratio Test P-values:\n")
        print(round(LRp, 5))
        if(sum(p20) > 0) {
            LRps <- matrix(LRp[p20 == 1, ], ncol = 1)
            rownames(LRps) <- names(Ws)[p20 == 1]
            colnames(LRps) <- "P-value"
            cat(paste("\n\nCovariates selected (P<", pval, "):\n", 
                sep = ""))
            print(LRps)
        }
        else{
            cat(paste("\nNo covariates were associated with the outcome at P<", 
                pval))
        }
    }
    
    W <- data.frame(wvar=names(Ws), p=as.numeric(LRp), pthres=as.numeric(p20))
    if(floor(ncases/10) > 0){
    W <- W %>% arrange(p) %>% slice(1:floor(ncases/10))
    }else{
      W$pthres = 0
      cat("\nNot enough cases for adjusted analysis")
    }
    return(as.character(W$wvar[W$pthres == 1]))
}



#-------------------------------------------------------------------
# Impute missingness function
#-------------------------------------------------------------------


Mode<-function(x){
  ux = unique(x)
  tab = tabulate(match(x, ux))
  ux[tab == max(tab)]
}

missingness_indicators<-function (data, prefix = "miss_", remove_constant = T, remove_collinear = T, 
                                  skip_vars = c(), verbose = F) {
  indicators = sapply(data[, !colnames(data) %in% skip_vars], 
                      FUN = function(col) as.numeric(is.na(col)))
  colnames(indicators) = paste0(prefix, colnames(data)[!colnames(data) %in% 
                                                         skip_vars])
  if (remove_constant) {
    col_means = colMeans(indicators)
    if (verbose) {
      num_removed = sum(col_means %in% c(0, 1))
      if (num_removed > 0) {
        cat("Removing", num_removed, "indicators that are constant.\n")
      }
    }
    indicators = indicators[, !col_means %in% c(0, 1), drop = F]
  }
  if (remove_collinear) {
    linear_combos = caret::findLinearCombos(indicators)
    remove_columns = linear_combos$remove
    if (length(linear_combos$remove) > 0) {
      if (verbose) {
        cat("Removing", length(linear_combos$remove), 
            "indicators due to collinearity:\n")
        cat(paste0(colnames(indicators)[linear_combos$remove], 
                   collapse = ", "), "\n")
      }
      indicators = indicators[, -linear_combos$remove, 
                              drop = F]
    }
  }
  return(indicators)
}




impute_missing_values<-function(data, type = "standard", add_indicators = T, prefix = "miss_", skip_vars = c(), verbose = F){
  missing_indicators = NULL
  new_data = data
  results = list(type = type, add_indicators = add_indicators, 
                 skip_vars = skip_vars, prefix = prefix)
  if (type == "standard") {
    if (verbose) {
      cat("Running standard imputation.\n")
    }
    preprocess = NA
    impute_values = vector("list", ncol(data))
    names(impute_values) = colnames(data)
    for (i in 1:ncol(data)) {
      nas = sum(is.na(data[[i]]))
      col_class = class(data[[i]])
      if (col_class == "factor") {
        impute_value = Mode(data[[i]])[1]
      }
      else if (col_class %in% c("integer", "numeric")) {
        impute_value = median(data[[i]], na.rm = T)
      }
      else {
        warning(paste(colnames(data)[i], "should be numeric or factor type. But its class is", 
                      col_class))
      }
      impute_values[[i]] = impute_value
      if (nas == 0 || names(data)[i] %in% skip_vars) {
        next
      }
      else if (nas == nrow(data)) {
        if (verbose) {
          cat("Note: skipping", colnames(data)[i], "because all values are NA.\n")
        }
        next
      }
      else {
        new_data[is.na(data[[i]]), i] = impute_value
      }
    }
    results$impute_values = impute_values
  }
  else if (type == "knn") {
    impute_info = caret::preProcess(new_data, method = c("knnImpute"))
    new_data = predict(impute_info, new_data)
    results$impute_info = impute_info
  }
  if (add_indicators) {
    missing_indicators = missingness_indicators(data, prefix = prefix, 
                                                verbose = verbose)
    if (verbose) {
      cat("Indicators added:", ncol(missing_indicators), 
          "\n")
    }
    new_data = cbind(new_data, missing_indicators)
  }
  results$data = new_data
  return(results)
}







#----------------------------------------------------
# Plotting functions
#----------------------------------------------------

#Theme
theme_set(theme_bw())

#Forest plot Color palette
cbPalette <- c( "#56B4E9" , rep("#999999",40))

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



#----------------------------------------------------
# Function to create fixed or random effects pooled RRs
#----------------------------------------------------

#Fit RE and FE models
meta_fun <- function(res, method, reference=NULL){
  
  #ID reference level
  levels<-levels(res$level)
  res<-res[!is.na(res$logRR.var),]
  if(is.null(reference)){
    reference<- levels[!(levels %in% unique(res$level))]
  }
  require(metafor)
  RMAest<-data.frame(study="Pooled estimate", res$variable[1],reference ,b=NA, se=NA)
  colnames(RMAest)<-c("study","variable","level","logRR.psi","logSE")

  for(j in 1:length(unique(res$level))){
    temp<-res[res$level==unique(res$level)[j],]
  fit<-rma(yi=logRR.psi, vi=logRR.var, data=temp, method=method, measure="RR")
  est<-data.frame(study="Pooled estimate", temp$variable[1],temp$level[1] ,fit$b, fit$se)
  colnames(est)<-c("study","variable","level","logRR.psi","logSE")
  RMAest<-rbind(RMAest, est)
  }
  
RMAest$RR<-exp(RMAest$logRR)
RMAest$RR.CI1<-exp(RMAest$logRR - 1.96 * RMAest$logSE)
RMAest$RR.CI2<-exp(RMAest$logRR + 1.96 * RMAest$logSE)

#rename SE column to var just to allow binding to original results:
#not used again in calculations
colnames(RMAest)[5]<-"logRR.var"
    return(RMAest)
}





#----------------------------------------------------
# Clean results dfs function
#----------------------------------------------------

cleandf <- function(d, meta_method="REML", RF_levels=c("<=2600","2600-3000","3000-3400",">3400")){
  
  d$study <- d$STUDYID
  d$country <- d$COUNTRY
  
  #remove grant identifier
  d$study<- gsub("^k.*?-" , "", d$study)
  
  #create labels for country-specific cohorts
  d$country <- as.character(d$country)
  d$country[d$country=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
  d$study <- paste0(d$study, " ", d$country)
  
  #drop studies with <5 cases in either reference or comparison level
  d <- d %>% filter(is.na(b) | !(b<5 | d<5))
  
  d_meantab <- d %>% subset(.,select=c(study,meanLevel:meanY, mean.CI1, mean.CI2))
  colnames(d_meantab) <- c("Study","Level", "Number in quartile", "Cumulative incidence of wasting", "95% CI-lower", "95% CI-upper")

  d <- d %>% subset(.,select=c(study, variable, level, logRR.psi, logRR.var,
                                               RR, RR.CI1, RR.CI2))
  
  #Estimate and merge in pooled RR's
  d_RE<-meta_fun(d, method=meta_method)
  d<-rbind(d_RE,d) %>% subset(., select=-c(logRR.psi,  logRR.var))

  # Clean dataframes for plotting
  #Sort levels/studies for plot
  rankedRR <- d %>% group_by(study) %>% summarize(maxRR = max(RR, na.rm=T)) %>% arrange(maxRR)
  rankedRR$order <- 1:nrow(rankedRR)
  d <- left_join(d, rankedRR, by="study")
  d$order[d$study=="Pooled estimate"] <- 1
  
  #Drop studies with no estimated RR and order by size of RR and arrange the dataframe
  d$level <- factor(d$level, levels=RF_levels)
  d <- d %>% filter(!(maxRR=="-Inf")) %>% arrange(order, level)
  d$x<-1:nrow(d)
  d$study <- factor(d$study , as.character(unique(d$study )))

  return(d)
}




#----------------------------------------------------
# RR Plot function
#----------------------------------------------------
RRplot_fun <- function(d, reflevel, title, units="", levels=NULL, free_Y=T){
  
  #change names of risk factor levels
  if(!is.null(levels)){
    d$level <- factor(as.numeric(d$level))
    levels(d$level) <- levels
  }
  
  #plot
n <- nrow(d)

plot_df <- d %>% filter(level!=reflevel)
plot_df$size<-ifelse(plot_df$study=="Pooled estimate",1,0.5)

RRplot<-ggplot(data=plot_df) + 
  labs(title = d$variable, x = "Cohort", y = paste0("Risk ratio (reference = ",reflevel," ", units,")")) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.125,0.25,0.5,1,2,4), trans='log10') +
  coord_flip() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
    scale_size_continuous(range = c(0.5, 1))+
  geom_pointrange( mapping=aes(x=study, y=RR, ymin=RR.CI1, ymax=RR.CI2, colour=study, size=size)) +
    theme(panel.border = element_blank(), 
    strip.background = element_blank())

if(free_Y){
  RRplot<-RRplot + facet_wrap(~level, ncol=1, scales = "free_y") 
}else{
  RRplot<-RRplot + facet_wrap(~level, ncol=1) 
}

RRplot<-RRplot + geom_vline(xintercept=1.5, color="grey20", linetype=2) +
   #geom_label(aes(label="Overall RR", x=1, y=6, size=.1)) + #Need to make smaller
    ggtitle(title) +theme(legend.position="none")#+ guides(colour = guide_legend(reverse=T)) 
return(RRplot)
}







#-----------------------------------
# Plot pooled RR's
#-----------------------------------


#Pooled plot function
scaleFUN <- function(x) sprintf("%.2f", x)

RF_metaplot <- function(d, title="", yticks=c(0.5, 0.6, 0.7,0.8,0.9,1, 1/0.9, 1/0.8, 1/0.7, 1/0.6, 2)){

  
  #get RR range to offset (ref.) by a relative amount in plot
d <- d %>% 
              group_by(variable) %>% 
              mutate(RRrange=diff(range(RR), na.rm=T),
                     reflabel="")
d$reflabel[is.na(d$RR)] <- "(ref.)"
d$RR[is.na(d$RR)] <- 1
  
  
p<-ggplot(d, aes(x=level)) + 
  geom_point(aes(y=RR, fill=variable, color=variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=variable),
                 alpha=0.5, size = 3) +
  geom_text(aes( y=0.95, label=reflabel, colour=variable)) +
  labs(x = "Risk factor level", y = "Cumulative Incidence Ratio") +
  geom_hline(yintercept = 1) +
  coord_cartesian(ylim=c(0.7, 1/0.6)) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  scale_size_continuous(range = c(0.5, 1))+
  theme(strip.background = element_blank(),
    legend.position="none",
    strip.text.x = element_text(size=12),
    axis.text.x = element_text(size=12)) +
  facet_wrap(~variable,  scales = "free_x") +
  ggtitle(title)

return(p)
}











