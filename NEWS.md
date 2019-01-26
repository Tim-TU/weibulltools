---
title: "NEWS"
subtitle: "Release of weibulltools v1.0.0"
output: html_document
---

#### Prerequisite for Package Usage:
* Since RcppArmadillo is used, the R version should be at least 3.3.0 
  (listed under Depends in DESCRIPTION file)

#### New Features: 
* Vignettes for non-parametric probability estimation, parameter estimation using 
  Median-Rank Regression and Maximum-Likelihood and mixture model estimation 
  are provided. 
* Argument _y_ in functions plot_prob_mix() and plot_mod_mix() is deprecated and 
  not used anymore. 
* Argument *reg_output* in functions plot_prob_mix() and plot_mod_mix() is 
  deprecated; use *mix_output* instead. 
* Function plot_mod_mix() was revised and updated in the way that the obtained 
  results of the function mixmod_em() can be visualized. 
* Function plot_prob_mix() was revised and updated in the way that the obtained 
  results of the function mixmod_em() can be visualized. 
* Implementation of EM-Algorithm using Newton-Raphson. The algorithm is written 
  in c++ (mixture_em_cpp()) and is called in mixmod_em(). 
* New method for the computation of Fisher's Confidence Bounds regarding 
  probabilities is used. These method is called "z-Procedure" and is more 
  appropriate to manage the bend-back behaviour. Therefore an adjustment of 
  functions delta_method() and confint_fisher() was made. 
* Implementation of log-location-scale models with threshold parameter like 
  three-parametric weibull ("weibull3"), three-parametric lognormal ("lognormal3") 
  and three-parametric loglogistic ("loglogistic3"). 
* Implementation of location-scale models like smallest extreme value ("sev"), 
  normal ("normal") and logistic ("logistic"). 
* Implementation of _Log-Likelihood Profiling_ for three-parametric models in 
  function loglik_profiling(). In general this function is used inside 
  ml_estimation() for purpose of estimating threshold parameter of 
  three-parametric models. 
* Implementation of _R-Squared Profiling_ for three-parametric models in function 
  r_squared_profiling(). In general this function is used inside rank_regression() 
  for purpose of estimating threshold parameter of three-parametric models.
* Implementation of _Log-Likelihood Function_ for all implemented models in function 
  loglik_function(). In general this function is used inside ml_estimation() for 
  purpose of estimating the variance-covariance matrix of location-scale models 
  "sev", "normal" and "logistic". The function is also used to estimate the 
  variance-covariance matrix of log-location-scale models with a threshold 
  parameter, i.e. "weibull3", "lognormal3" and "loglogistic3".
* new argument in function ml_estimation(): _wts_ for case weights. 
