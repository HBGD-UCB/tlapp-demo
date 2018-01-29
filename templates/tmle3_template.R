## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = FALSE, message=FALSE, eval.after = 'fig.cap')
options(scipen=999)

## ----params, warning=FALSE-----------------------------------------------
library(tltools)
library(tmle3)
library(sl3)
library(ggplot2)
library(skimr)
data <- get_tl_data()
nodes <- get_tl_nodes()
library(future)
tl_params <- get_tl_params()

# force parameter values for testing purposes
tl_params$parallelize=FALSE
tl_params$library_type="comprehensive"

if(tl_params$parallelize){
  
  workers=availableCores()/2
  plan(multicore, workers=workers)
} else {
  workers = 1
  plan(sequential)
}



## ----data----------------------------------------------------------------
#todo: handle missingness better
data[is.na(data)] <- 0

#discritize A
#todo: handle multiple A
A_node <- nodes$A[1]
A_vals <- unlist(data[,A_node, with = FALSE])
if(!is.factor(A_vals)){
  num_cats <- as.numeric(tl_params$num_treatment_categories)
  #todo: make cuts script_param
  quants <- seq(from=0,to=1,length=num_cats+1)
  cuts <- quantile(A_vals, quants)
  A_factor <- cut(A_vals, cuts, right = FALSE, include.lowest = TRUE)
  data[, A_node] <- A_factor
}


## ----define_learners-----------------------------------------------------
if(tl_params$library_type=="mean"){
  qlib <- glib <- make_learner_stack("Lrnr_mean")
} else{
  #todo: some kind of clean SL wrapper fun (with defaults!)
  qlib <- make_learner_stack("Lrnr_mean", 
                             "Lrnr_glm_fast",
                             "Lrnr_glmnet",
                             list("Lrnr_xgboost", nthread=1))
  
  glib <- make_learner_stack("Lrnr_mean",
                             "Lrnr_glmnet",
                             list("Lrnr_xgboost", nthread=1))
}


# qlib <- glib <- make_learner_stack("Lrnr_mean")
mn_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_loglik_multinomial, learner_function = metalearner_linear_multinomial)
metalearner <- make_learner(Lrnr_nnls)
Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, mn_metalearner)

learner_list <- list(Y=Q_learner, A=g_learner)

## ----fit_tmle------------------------------------------------------------
tmle_fit <- tmle3(tmle_tsm_all(), data, nodes, learner_list)

## ----nodes---------------------------------------------------------------
print(nodes)

## ----data_summary, results="asis"----------------------------------------
tmle_task <- tmle_fit$tmle_task
skim_format(.levels = list(max_char = 10))
skimr::kable(summarize_tmle_data(tmle_task))

## ----fit_likelihood------------------------------------------------------
likelihood <- tmle_fit$likelihood

## ----inspect_fits--------------------------------------------------------
# todo: function to extract sl3 learner fits from likelihood object
Q_fit <- likelihood$factor_list[["Y"]]$learner
Q_risk <- Q_fit$cv_risk(loss_squared_error)
g_fit <- likelihood$factor_list[["A"]]$learner
g_risk <- g_fit$cv_risk(loss_loglik_multinomial)

## ----q_fit---------------------------------------------------------------
# todo: fix the label on this so it reflects what was actually fit
kable(Q_risk)

## ----q_pred_plot---------------------------------------------------------
prediction_plot(Q_fit)

## ----g_fit---------------------------------------------------------------
kable(g_risk)

## ----g_pred_plot---------------------------------------------------------
prediction_plot(g_fit)

## ----propensity_plot-----------------------------------------------------
  propensity_score_plot(likelihood, tmle_task, "A")

## ----propensity_table----------------------------------------------------
  kable(propensity_score_table(likelihood, tmle_task, "A"), 
        digits=4)

## ----tmle_estimate_plot--------------------------------------------------
  plot(tmle_fit)

## ----contrasts-----------------------------------------------------------
#todo: generalize delta method code
baseline <- as.numeric(tl_params$baseline_category)
others <- setdiff(seq_len(as.numeric(tl_params$num_treatment_categories)), baseline)
estimates <- tmle_fit$estimates
param_names <- tmle_fit$tmle_param_names
contrasts <- lapply(others, function(contrast_level){
  estimate <- delta_method(estimates[c(baseline,contrast_level)], f_contrast, f_contrast)
  summary_from_estimate(estimate)
})
contrast_params <- sprintf("%s - %s", param_names[others], param_names[baseline])
contrast_dt <- rbindlist(contrasts)
contrast_dt$param <- contrast_params
ggplot(contrast_dt, aes(y=param, x=tmle_est, xmin=lower, xmax=upper))+
  geom_point()+geom_errorbarh()+theme_bw()+xlab("Value")+ylab("Parameter")

## ----save_tmle_summary---------------------------------------------------
summary <- tmle_fit$summary
summary_file <- file.path(params$output_directory, "tmle_summary.rdata")
save(summary, file=summary_file)

## ----timings-------------------------------------------------------------
kable(as.data.frame(tmle_fit$timings[, "elapsed"]), col.names = "Time (seconds)")

