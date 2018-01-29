



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
    
    
    
    mixedCV.tmle.A<-tmle(Y=Y[,1], 
                         A=subset(d, select=Avar)[,1], 
                         W=X[,-1], 
                         Q=Q,
                         g1W=g1W,
                         family = family, 
                         verbose = F)
    
  }else{
    mixedCV.tmle.A<-tmle(Y=Y[,1], 
                         A=subset(d, select=Avar)[,1], 
                         W=W, 
                         family = family, 
                         Q.SL.library=lib,
                         g.SL.library = lib,
                         verbose = F,
                         id=subjid[,1])  
    
  }
  if(family=="binomial"){
    return(c(unlist(mixedCV.tmle.A$estimates$ATE),unlist(mixedCV.tmle.A$estimates$RR)))
  }else{
    return(unlist(mixedCV.tmle.A$estimates$ATE))
  }
}


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
 
  
  #get study name
  study <-deparse(substitute(dat))
  
  #get number of studies 
  nstudies<-length(unique(dat$STUDYID))
  
  if(A %in% W){W<-W[-which(W %in% A)]}
  y<-subset(dat, select=Y) %>% as.data.frame()
  a<-subset(dat, select=A) %>% as.data.frame()
  dat$STUDYID<-as.factor(dat$STUDYID)
  studyid<-subset(dat, select="STUDYID") %>% as.data.frame()
  subjid<-subset(dat, select="SUBJID") %>% as.data.frame()
  
  
  if(overall.dist==F & class(a[,1])=="numeric"){
    Acuts<-as.numeric(quantile(dat$A, probs = c((1:(n.cat-1))/n.cat), na.rm=T))
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
    w <- w[, sapply(w, nlevels) > 1]
    preproc = caret::preProcess(w, method = c("zv", "nzv"))
    w = predict(preproc, w)
    dim(w)
    print(colnames(w))
  }

  
  
  #Make sure at least 2 columns in w for glmnet
  if(ncol(w)<2){w<-data.frame(w, nullvar=rep(1, nrow(w)))}
  
  
  fulldat<-data.frame(y,a,studyid,subjid,w)
  fulldat<-fulldat[complete.cases(fulldat),]
  
  #Extract mean Y|A
  levelmeans<- fulldat %>% #fulldat[fulldat[,2]==levels(fulldat[,2])[1],] %>%
    group_by(.[[2]]) %>%
    do(as.data.frame(washb_mean(Y=.[[1]], id=1:length(.[[1]]), print = F))) %>% 
    as.data.frame %>% `rownames<-`(.[,1]) #%>% .[,-1]
  
  
  #Extract desired levels
  levelmeans<-levelmeans[1:n.cat,]
  
 
  
  res<-NULL
  for(i in comparisons){
    
    dat<-fulldat[fulldat[,2]==reference | fulldat[,2]==i,]

    print(table(dat[,2]))
    
    #Convert factor to binary indicator
    dat[,2]<-ifelse(dat[,2]==reference,0,1)
    
    sparse=F
    if(family=="binomial"){
      print(c(table(dat[,1],dat[,2])))
      tab<-c(table(dat[,1],dat[,2]))
      if(tab[1]<sparseN+1 | tab[2]<sparseN+1  | tab[3]<sparseN+1  | tab[4]<sparseN+1 | is.na(tab[1]) | is.na(tab[2]) | is.na(tab[3]) | is.na(tab[4])){
        sparse=T
      }
    }
    
    
    
    if(sum(dat[,2]==0)>sparseN & sum(dat[,2]==1)>sparseN & sparse==F){ #Make sure there is enough support in the data
      w<-as.data.frame(dat[,5:ncol(dat)])
      if(ncol(w)==1){colnames(w)<-colW}
      
      #likelihood ratio test prescreening of covariates
      if(adjusted==T){
        Wscreen <- washb_prescreen(Y=dat[,1], Ws=w)
        if(length(Wscreen) > 0){ 
          w<-subset(w, select=Wscreen) 
          w<-as.data.frame(design_matrix(w))
          if(ncol(w)==1){w <- data.frame(W1=w[,1], nullvar1=rep(1, nrow(dat)))}
        }else{
          w<-data.frame(nullvar1=rep(1, nrow(dat)),nullvar2=rep(1, nrow(dat)))
        }
      }
      
      
      df <- data.frame(dat[,1:4],w)
      
      fit<-studyCV.tmle(d=df, 
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
  
  res<-cbind(study,res,levelmeans)
  
  if(family=="binomial"){
    colnames(res)<-c("study","variable","level","ATE","ATE.var","ATE.CI1","ATE.CI2", "ATE.Pval","RR","RR.CI1","RR.CI2", "RR.Pval", "logRR.psi","logRR.var","reference", "compN", "refN","a","b","c","d", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }else{
    colnames(res)<-c("study","variable","level","ATE","var","CI1","CI2", "Pval","reference", "compN", "refN", "meanLevel", "meanN",
                     "meanY", "mean.sd","mean.se","mean.CI1","mean.CI2")
  }
  rownames(res)<-NULL
  res<-res[,-1] #Drop study label for dplyr groupby code
  
  if(!is.null(outputdf)){
    return(rbind(outputdf,res))
  }else{
    return(res)
  }
}












loadGHAP<-function(study, varlist){
  
  d<-readRDS(paste0(study,".rds"))
  
  d<-d[,which(colnames(d) %in% varlist)]
  #d <- lapply(d,as.factor)
  d <- apply(d, 2, as.character)
  d<-as.data.frame(d)
  return(d)
}



bindGHAP_Fill<-function(study, varlist=NULL, d=d){
  
  study.d<-readRDS(paste0(study,".rds"))
  
  if(!is.null(varlist)){
    study.d<-study.d[,which(colnames(study.d) %in% varlist)]
  }
  
  study.d <- apply(study.d, 2, as.character)
  study.d<-as.data.frame(study.d)
  
  #Set "" and other missing codes to missing
  for(i in 1:ncol(study.d)){
    study.d[,i]<-ifelse(study.d[,i]=="",NA,as.character(study.d[,i]))
  } 
  study.d<-study.d %>%  
    group_by(SUBJID) %>%
    do(fill(.,everything())) %>% 
    do(fill(.,everything(), .direction = 'up')) 
  
  
  
  study.d$AGEDAYS<-as.numeric(as.character(study.d$AGEDAYS))
  study.d$HAZ<-as.numeric(as.character(study.d$HAZ))
  study.d$WHZ<-as.numeric(as.character(study.d$WHZ))
  
  study.d <-study.d %>% group_by(SUBJID) %>% 
    filter(!is.na(HAZ)) %>%
    slice(which.min(abs(AGEDAYS - 548))) %>% #Find observation with age closest to 1.5 year old
    filter(AGEDAYS > 90 & AGEDAYS < 1100) %>% #Select ages between 3 months and 3.5 years
    ungroup
  
  
  d<- bind_rows(d, study.d , .id = "studyid")
  rm(study.d)
  return(d)
}




design_matrix<-function (W) 
{
  if (class(W) != "matrix" & class(W) != "data.frame") {
    W <- data.frame(W)
    if (is.null(ncol(W)) | ncol(W) == 0) {
      stop("Something is wrong with W.\nTo be safe, please try specifying it as class=data.frame.")
    }
  }
  ncolW <- ncol(W)
  flist <- numeric()
  for (i in 1:ncolW) {
    if (class(W[, i]) != "factor") {
      next
    }
    else {
      flist <- c(flist, i)
      W[, i] <- factor(W[, i])
      mm <- model.matrix(~-1 + W[, i])
      mW <- mm[, -c(1)]
      levs <- gsub(" ", "", levels(W[, i]))[-c(1)]
      if (length(levs) < 2) 
        mW <- matrix(mW, ncol = 1)
      colnames(mW) <- paste(names(W)[i], levs, sep = "")
      W <- data.frame(W, mW)
    }
  }
  if (length(flist) > 0) {
    W <- W[, -c(flist)]
  }
  return(W)
}







missing.data.SLimpute<-function(Y, d,family="gaussian", lib){
  
  #Missingness indicator
  Xmiss<-ifelse(is.na(d[,Y]),1,0)
  
  #Drop predictors with missingness
  dY<-subset(d, select=Y)
  dX<-d[,!(names(d) %in% Y)] 
  
  miss.col<-rep(T,ncol(dX))
  for(i in 1:ncol(dX)){
    if(sum(is.na(dX[,i]))!=0){
      miss.col[i]<-F
    }
  }
  
  d.temp<-subset(dX, select= colnames(dX)[which(miss.col==F)])
  #d.temp<-as.data.frame(dX[,!miss.col])
  dX<-dX[,miss.col]
  
  d<-cbind(dY,dX)
  train<-subset(d, !is.na(d[,Y]))
  predict<-subset(d,is.na(d[,Y]))
  
  
  mi<-which(is.na(d[,Y])) #missing index
  
  
  #Set up x and y variables for SL model
  X<-setdiff(names(d), Y)  
  
  
  fit<- SuperLearner(Y=as.numeric(subset(train, select=Y)[,1]), 
                     X=subset(train, select=X), 
                     family = gaussian(), SL.library=lib)
  
  
  # Generate predictions on the missing data set
  pred <- predict.SuperLearner(fit, newdata=subset(predict, select=X))
  d[mi,Y]<- as.data.frame(pred$pred)
  
  d<-cbind(d.temp,d,Xmiss)
  colnames(d)[ncol(d)]<-paste0(Y,".miss")
  
  return(d)
}




prepW<-function(study, Wvars, cont.vars=c("AGEDAYS")){

  
  d<-as.data.frame(study)
  
  W<-subset(d, select=Wvars)
  table(is.na(W))
  for( i in 1:ncol(W)){
    cat(i, " ",colnames(W)[i],": ", class(W[,i]),"\n")
  }
  
  W <- apply(W, 2, as.character)
  W[W=="" | W=="." | W=="NA" | is.na(W)]<-"Missing"
  
  
  #Split out continious and factor variables
  cont.vars.index<-which(colnames(W) %in% cont.vars)
  contW<-W[,cont.vars.index]
  
  if(!is.null(ncol(contW))){
    suppressWarnings(contW <- apply(contW, 2, as.numeric))
  }else{
    suppressWarnings(contW<-as.numeric(contW))
  }
  
  factW<-W[,-cont.vars.index]
  factW <- apply(factW, 2, as.factor)
  
  table(is.na(factW))
  
  
  
  
  #Impute missing continious data
  #Add back in factor data for imputation
  data<-data.frame(contW, factW)
  table(is.na(data))
  for( i in 1:ncol(data)){
    cat(i, " ",colnames(data)[i],": ", class(data[,i]),"\n")
  }
  
  
  #Then SL impute continious variables
  for(i in 1:length(cont.vars)){
    suppressWarnings(data<-missing.data.SLimpute(Y=cont.vars[i], d=data, family="gaussian", lib=c("SL.mean","SL.glmnet")))
  }
  table(is.na(data))
  
  
  #Convert factors to indicators
  W<-design_matrix(data)
  
  #Remove near zero variance columns
  dim(W)
  preproc = caret::preProcess(W, method = c("zv", "nzv"))
  W = predict(preproc, W)
  rm(preproc)
  dim(W)
  
  #Drop empty factor levels
  W<-droplevels(W)
  
  return(W)
}







select_groups <- function(data, groups, ...) {
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]}










#---------------------------------------
#
# Asset PCA function
#
# The analysis script for the HBGDki
# analysis - creation of iLins-Dyad-M 
# wealth factor analysis 
#---------------------------------------


assetPCA<-function(dfull, varlist, reorder=F ){
  
  varlist<-c("SUBJID",varlist)
  
  #Subset to only needed variables for subgroup analysis
  ret <- dfull %>%
    subset(select=c(varlist))
  
  for(i in 1:ncol(ret)){
    ret[,i]<-ifelse(ret[,i]=="",NA,ret[,i])
  } 
  
  #drop rows with no asset data
  ret<-ret[rowSums(is.na(ret[,2:ncol(ret)])) != ncol(ret)-1,]  
  
  #PCA of asset based wealth by enrollment
  #Method based on: https://programming-r-pro-bro.blogspot.com/2011/10/principal-component-analysis-use.html
  
  #Select assets
  ret<-as.data.frame(ret) 
  id<-subset(ret, select=SUBJID) #drop subjectid
  ret<-subset(ret, select=-SUBJID) #drop subjectid
  
  
  #Drop assets with great missingness
  for(i in 1:ncol(ret)){
    cat(colnames(ret)[i],"\n")
    print(table(is.na(ret[,i])))
    print(class((ret[,i])))
  }
  
  #Set missingness to zero
  table(is.na(ret))
  for(i in 1:ncol(ret)){
    ret[,i]<-as.character(ret[,i])
    ret[is.na(ret[,i]),i]<-"miss"
    ret[,i]<-as.factor(ret[,i])
    
  }
  table(is.na(ret))
  
  #Remove columns with almost no variance
  if(length(nearZeroVar(ret))>0){
    ret<-ret[,-nearZeroVar(ret)]
  }
  
  #Convert factors into indicators
  ret<-droplevels(ret)
  ret<-design_matrix(ret)
  
  #Set missingness to zero
  table(is.na(ret))
  ret[is.na(ret)]<-0
  table(is.na(ret))
  
  #Remove columns with almost no variance
  if(length(nearZeroVar(ret))>0){
    ret<-ret[,-nearZeroVar(ret)]
  }
  
  ## Convert the data into matrix ##
  ret<-as.matrix(ret)
  
  
  # 
  ##Computing the principal component using eigenvalue decomposition ##
  princ.return <- princomp(ret) 
  
  
  ## To get the first principal component in a variable ##
  load <- loadings(princ.return)[,1]   
  
  pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
  
  HHwealth <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
  
  #Create 4-level household weath index
  quartiles<-quantile(HHwealth, probs=seq(0, 1, 0.25))
  ret<-as.data.frame(ret)
  ret$HHwealth_quart<-rep(1, nrow(ret))
  ret$HHwealth_quart[HHwealth>=quartiles[2]]<-2
  ret$HHwealth_quart[HHwealth>=quartiles[3]]<-3
  ret$HHwealth_quart[HHwealth>=quartiles[4]]<-4
  table(ret$HHwealth_quart)
  ret$HHwealth_quart<-factor(ret$HHwealth_quart)
  
  if(reorder==T){
    levels(ret$HHwealth_quart)<-c("Wealth Q4","Wealth Q3","Wealth Q2","Wealth Q1")
    ret$HHwealth_quart<-factor(ret$HHwealth_quart, levels=c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4"))
  }else{
    levels(ret$HHwealth_quart)<-c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4")
  }
  
  #Table assets by pca quartile to identify wealth/poverty levels
  d<-data.frame(id, ret)
  wealth.tab <- d %>% subset(., select=-c(SUBJID)) %>%
    group_by(HHwealth_quart) %>%
    summarise_each(funs(mean)) %>% as.data.frame()
  print(wealth.tab)
  
  #Save just the wealth data
  pca.wealth<-d %>% subset(select=c(SUBJID,  HHwealth_quart))
  
  pca.wealth$SUBJID<-as.numeric(as.character(pca.wealth$SUBJID))
  
  dfull <-dfull %>% subset(., select=SUBJID)
  dfull$SUBJID<-as.numeric(as.character(dfull$SUBJID))
  d<-left_join(dfull, pca.wealth, by="SUBJID")
  return(d)
}










rmvn <- function(n, mu, sig) { ## MVN random deviates
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(m*n), m, n))
}



GAM_simulCI<-function (Y, Age, W = NULL, id = NULL, SL.library = c( "SL.gam"), cvControl = list(V=5), 
                       gamdf = NULL, imputeX=F){
  
  require(SuperLearner)
  if(is.null(id)) 
    id <- 1:length(Y)
  if(is.null(W)){
    nullW <- TRUE
    fulld <- data.frame(id, Y, Age)
  }else{
    nullW <- FALSE
    Wdesign <- design_matrix(W)
    fulld <- data.frame(id, Y, Age, Wdesign)
  }
  if(imputeX==T){
    As <- seq(0, max(fulld$Age), by=0.1)
  }else{
    As <- unique(fulld$Age)
  }
  pY <- rep(NA, length(As))
  fitd <- fulld[complete.cases(fulld), ]
  n.orig <- dim(fulld)[1]
  n.fit <- dim(fitd)[1]
  
  #test of transformation
  #fitd$Y<-log(fitd$Y+0.1)
  # fitd$Y<-ifelse(fitd$Y==0,0.0001,0.9999)
  # fitd$Y<-qlogis(fitd$Y)
  
  
  if (n.orig > n.fit) 
    warning(paste("\n\n", n.orig - n.fit, "observations were dropped due to missing values\n in the outcome, age, or adjustement covariates. \n The original dataset contained", 
                  n.orig, "observations,\n but GAM_simulCI is fitting the curve using", 
                  n.fit, "observations."))
  X <- subset(fitd, select = -c(1:2))
  if (length(grep("SL.gam", SL.library)) > 0) {
    set.seed(123456)
    cvGAM <- ab_cvGAM(Y = fitd$Y, X = X, id = fitd$id, SL.library = SL.library, 
                      cvControl = cvControl, df = gamdf)
    SL.library <- cvGAM$SL.library
  }
  # SLfit <- SuperLearner::SuperLearner(Y = fitd$Y, X = X, id = fitd$id, 
  #     cvControl = cvControl, SL.library = SL.library, family = "gaussian", 
  #     method = "method.NNLS")
  # p_res <- cbind(SLfit$cvRisk, SLfit$coef)
  # colnames(p_res) <- c("CV-Risk", "Coef")
  # cat("\nSummary of SuperLearner cross validated risk and \nweights for algorithms included in the library:\n\n")
  # print(p_res)
  # 
  try(detach(package:gam))
  require(mgcv)
  #m <- gam(Y ~ s(Age, k = cvGAM$df_opt), data = fitd, family="binomial",  method = "REML")
  m <- gam(Y ~ s(Age, k = cvGAM$df_opt), data = fitd,  method = "REML")
  pval<- unlist(summary(m))$s.pv
  #     for (i in 1:length(As)) {
  #         X$Age <- As[i]
  #         pYs <- predict(SLfit, newdata = X)$pred
  #         pY[i] <- mean(pYs)
  #     }
  #     res <- merge(fitd, data.frame(Age = As, pY = pY), by = "Age", 
  #         all.x = T, all.y = T)
  # 
  # res <- res[order(res$Age), ]
  # list(pY = res$pY, Age = res$Age, Y = res$Y, W = subset(fitd, 
  #     select = -c(1:3)), id = res$id, fit = SLfit)
  
  
  Vb <- vcov(m)
  newd <- seq(min(Age), max(Age), length = nrow(fitd))
  pred <- predict(m, data.frame(Age = newd),  se.fit = TRUE)
  se.fit <- pred$se.fit
  
  
  set.seed(123456)
  N <- 10000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  Cg <- predict(m, data.frame(Age = newd), type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  
  
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  
  masd <- apply(absDev, 2L, max)
  crit <- quantile(masd, prob = 0.95, type = 8)
  pred <- transform(cbind(data.frame(pred), newd),
                    uprP = fit + (2 * se.fit),
                    lwrP = fit - (2 * se.fit),
                    uprS = fit + (crit * se.fit),
                    lwrS = fit - (crit * se.fit))  
  
  pred<-data.frame(Y=fitd$Y, X=fitd$Age, pred, Pval=rep(pval, nrow(fitd)))  
  
  # pred$Y<-round(exp(pred$Y)-.1,3)
  # pred$fit<-round(exp(pred$fit)-.1,3)
  # pred$uprS<-round(exp(pred$uprS)-.1,3)
  # pred$lwrS<-round(exp(pred$lwrS)-.1,3)
  
  # pred$Y<-plogis(pred$Y)
  # pred$fit<-plogis(pred$fit)
  # pred$uprS<-plogis(pred$uprS)
  # pred$lwrS<-plogis(pred$lwrS)
  return(pred)    
}










ab_cvGAM<-function (Y, X, id = NULL, family = gaussian(), SL.library, cvControl = list(), 
                    print = FALSE, df = 2:10) 
{
  if (is.null(df)) {
    df <- 2:10
  }
  if (print == TRUE) {
    cat("\nThe ensemble library includes SL.gam.")
    cat("\nThe default R implementation of gam() may over- or under-smooth the data")
    cat("\nTuning the fit by selecting the optimal df for the smoothing splines")
    cat("\nfrom ", df[1], " to ", df[length(df)], " using V-fold cross-validation.")
  }
  require(SuperLearner)
  if (is.null(id)) 
    id <- 1:length(Y)
  create.SL.gam <- function(tune = list(df = df)) {
    for (mm in seq(length(tune$df))) {
      eval(parse(file = "", text = paste("SL.gam.df", tune$df[mm], 
                                         "<- function(...,deg.gam = ", tune$df[mm], ") SL.gam(..., deg.gam = deg.gam)", 
                                         sep = "")), envir = .GlobalEnv)
    }
    invisible(TRUE)
  }
  create.SL.gam()
  cvRisks <- rep(NA, length(df))
  for (nn in seq(length(df))) {
    fit <- SuperLearner(Y = Y, X = X, id = id, family = family, 
                        SL.library = paste("SL.gam.df", df[nn], sep = ""), 
                        cvControl = cvControl)
    cvRisks[nn] <- fit$cvRisk
  }
  df_opt <- df[order(cvRisks)][1]
  cvr_tab <- cbind(df, cvRisks)
  colnames(cvr_tab) <- c("df", "CVRisk")
  SLlib2 <- gsub("SL.gam", paste("SL.gam.df", df_opt, sep = ""), 
                 SL.library)
  if (print == TRUE) {
    cat("\n-----------------------------------")
    cat("\nGeneralized additive model with natural splines\n")
    cat("\nOptimal smoothing df: ", df_opt, "\n")
    print(cvr_tab)
    cat("-----------------------------------\n")
  }
  return(list(SL.library = SLlib2, df_opt = df_opt, cvRisks = cvr_tab))
}















