# weibulltools v1.1.0
## Breaking Changes
* Renamed argument `event` with `status`.
* Output of probability estimators: Renamed column `characteristic` with `x`.
* `plot_conf()`: Switched arguments distribution and direction.
* `rank_regression()`, `ml_estimation()`: Removed details argument.
* `rank_regression()`, `ml_estimation()`: Renamed output: `loc_sc_coefficients` -> `loc_sc_params`, `loc_sc_vcov` -> `loc_sc_varcov`
* `plot_pop()`: Added argument `tol` to restrict the range of failure probabilities. Removed argument `color`. Renamed argument `params` to `loc_sc_params_tbl`, which only supports location and scale parameters (also for `distribution = "weibull"`). Changed behaviour of `loc_sc_params_tbl`: A tibble is now recommended instead of a vector.
* `plot_prob_mix`: Removed default value `NULL` for argument `mix_output`.

## New Features
* Added support for ggplot2 in all plot functions. Plot method can be selected in `plot_prob()` via argument `plot_method`.
* Added function `reliability_data()`: Create consistent reliability data.
* New argument in `mr_method()`: With `ties.method` it can be specified how ties should be treated.
* Added `estimate_cdf()`: Unite functionality of `mr_method()`, `johnson_method()`, `kaplan_method()` and `nelson_method()`. Support multiple methods.
* `rank_regression()` is now an S3 generic. `rank_regression()` becomes `rank_regression.default()`. Added `rank_regression.cdf_estimation()`.
* `plot_prob()` is now an S3 generic. `plot_prob()` becomes `plot_prob.default()`. Added `plot_prob.cdf_estimation()`.
* `plot_prob_mix()` is now an S3 generic. `plot_prob_mix()` becomes `plot_prob.default()`. Added `plot_prob_mix.model_estimation()`, `plot_prob_mix.mixmod_regression()` and `plot_prob_mix.mixmod_em()`.
* `plot_mod()` is now an S3 generic. `plot_mod()` becomes `plot_mod.default()`. Added `plot_mod.model_estimation()`.
* `plot_conf()` is now an S3 generic. `plot_conf()` becomes `plot_conf.default()`. Added `plot_conf.confint()`.
* `plot_pop()`: Added support for multiple population lines and comparison of two- and three-parametric distributions.
* `confint_betabinom()`, `confint_fisher()`: Added argument `b_lives` which allows the user to specify probabilities of interest.
* Added `print.rank_regression()`, `print.ml_estimation()`, `print.model_estimation()` and `print.model_estimation_list()`.
* Added `vcov.model_estimation()`.
* Added function `dist_delay()`: Generalizes the distribution-specific modeling of delays. 
* Added function `mcs_delay()`: Generalizes the adjustment of operating times by delays.
* Added lifecycle badges

## Documentation
* Capitalized parameter documentation.

## Lifecycle changes

### Deprecated
* `plot_layout()`
* `mr_method()`, `johnson_method()`, `kaplan_method()` and `nelson_method()`: Use `estimate_cdf()` instead.
* `dist_delay_register()` and `dist_delay_report()` are deprecated. Use `dist_delay()` instead. 
* `mcs_delay_register()`, `mcs_delay_report()` and `mcs_delays()`  are deprecated. Use `mcs_delay()` instead.
## Minor improvements and bug fixes
* Fixed bug inside `plot_mod_mix()` for the case of no mixture distribution
* Fixed bug inside `confint_betabinom()`; many cases near one -> unique()
* Fixed bug inside `mr_method()`: Assigning a rank for the same lifetime 
* Added trace type "scatter" and scatter mode "markers" to `plot_layout`.

# weibulltools v1.0.1
* Fixed installation error when using clang compiler

# weibulltools 1.0.0 

## Prerequisite for Package Usage:

* Since RcppArmadillo is used, the R version should be at least 3.3.0 
  (listed under Depends in DESCRIPTION file)
  
## Changes

* Vignettes for non-parametric probability estimation, parameter estimation using Median-Rank Regression and Maximum-Likelihood and mixture model estimation are provided. 
* Argument _y_ in functions `plot_prob_mix()` and `plot_mod_mix()` is deprecated and not used anymore. 
* Argument *reg_output* in functions `plot_prob_mix()` and `plot_mod_mix()` is deprecated; use *mix_output* instead. 
* Function `plot_mod_mix()` was revised and updated in the way that the obtained results of the function `mixmod_em()` can be visualized. 
* Function `plot_prob_mix()` was revised and updated in the way that the obtained results of the function `mixmod_em()` can be visualized. 
* Implementation of EM-Algorithm using Newton-Raphson. The algorithm is written in c++ (`mixture_em_cpp()`) and is called in `mixmod_em()`. 
* New method for the computation of Fisher's Confidence Bounds regarding probabilities is used. These method is called "z-Procedure" and is more appropriate to manage the bend-back behaviour. Therefore an adjustment of functions `delta_method()` and `confint_fisher()` was made. 
* Implementation of log-location-scale models with threshold parameter like three-parametric weibull ("weibull3"), three-parametric lognormal ("lognormal3") and three-parametric loglogistic ("loglogistic3"). 
* Implementation of location-scale models like smallest extreme value ("sev"), normal ("normal") and logistic ("logistic"). 
* Implementation of _Log-Likelihood Profiling_ for three-parametric models in function `loglik_profiling()`. In general this function is used inside `ml_estimation()` for the purpose of estimating threshold parameter of three-parametric models. 
* Implementation of _R-Squared Profiling_ for three-parametric models in function `r_squared_profiling()`. In general this function is used inside `rank_regression()` for the purpose of estimating threshold parameter of three-parametric models.
* Implementation of _Log-Likelihood Function_ for all implemented models in function `loglik_function()`. In general this function is used inside `ml_estimation()` for the purpose of estimating the variance-covariance matrix of location-scale models "sev", "normal" and "logistic". The function is also used to estimate the variance-covariance matrix of log-location-scale models with a threshold parameter, i.e. "weibull3", "lognormal3" and "loglogistic3".
* new argument in function `ml_estimation()`: _wts_ for case weights. 
