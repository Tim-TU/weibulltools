# weibulltools (development version)

## Breaking Changes
* `confint_betabinom()` and `confint_fisher()`: Removed constant features `distribution`, `bounds` and `direction` from the tibble output and added them as attributes instead.
* `plot_prob.wt_model()`: Removed dysfunctional argument `distribution`. The distribution is inferred using the model `x`.

## New Features
* `confint_betabinom()`: Methods `"kaplan"` and `"nelson"` of `estimate_cdf()` can be used for beta-binomial confidence bounds. 

## Minor Improvements and bug fixes
* Fixed bug in `plot_conf()`: Wrong confidence bounds were displayed for `direction = "x"` (#181). 
* Fixed bug in `plot_conf()`: `plot_method = "ggplot2"` and exactly one method in `estimate_cdf()` resulted in an error (#182).
* Fixed bug in `reliability_data()`: Using `!!` syntax with arguments `x` and `status` resulted in an error.

## Documentation improvements
* `plot_prob()`: Better work out the distinction between `plot_prob.wt_cdf_estimation()` and `plot_prob.wt_model()`. The former is applied to a CDF estimation whereas the latter is applied to a mixture model.

# weibulltools v2.0.0
## Breaking Changes
* Package now depends on R(>= 3.5.0)

### Non-Parametric Failure Probabilities
* `mr_method()`: Deprecated, use `estimate_cdf()` instead. Renamed output column `characteristic` with `x`. Set default value for `id` to `NULL`.
* `johnson_method()`: Deprecated, use `estimate_cdf()` instead. Renamed output column `characteristic` with `x`. Set default value for `id` to `NULL`.
* `kaplan_method()`: Deprecated, use `estimate_cdf()` instead. Renamed output column `characteristic` with `x`. Set default value for `id` to `NULL`.
* `nelson_method()`: Deprecated, use `estimate_cdf()` instead. Renamed output column `characteristic` with `x`. Set default value for `id` to `NULL`.
* `plot_prob.default()` (former `plot_prob()`): Renamed `event` with `status`.
* `plot_prob_mix()`: Deprecated, use `plot_prob()` instead. Removed default value `NULL` for argument `mix_output`. Renamed `event` with `status`.

### Parametric Models
* `ml_estimation.default()` (former `ml_estimation()`): Renamed `event` with `status`. Removed `details`. Changed names and contents of list elements in output. See `?ml_estimation`.
* `loglik_function`: Renamed `event` with `status`. Renamed `pars` with `dist_params`.
* `rank_regression.default()` (former `rank_regression()`): Renamed `event` with `status`. Removed `details`. Changed names and contents of list elements in output. See `?rank_regression`.
* `mixmod_em.default()` (former `mixmod_em()`): Renamed `event` with `status`. Removed `post`.
* `mixmod_regression.default()` (former `mixmod_regression()`): Renamed `event` with `status`. Added arguments `k` and `control`, which provide finer control over the segmentation process. Expect default setting to provide other results than in prior versions.
* `predict_prob()`: Renamed `loc_sc_params` with `dist_params`.
* `predict_quantile()`: Renamed `loc_sc_params` with `dist_params`.
* `plot_mod.default()` (former `plot_mod()`): Renamed `event` with `status`. Renamed `loc_sc_params` with `dist_params`. Removed `y`.
* `plot_mod_mix()`: Deprecated, use `plot_mod()` instead. Renamed `event` with `status`.
* `plot_pop()`: Added argument `tol` to restrict the range of failure probabilities. Removed argument `color`. Renamed argument `params` to `dist_params_tbl`, which only supports location and scale parameters (also for `distribution = "weibull"`). Changed behavior of `dist_params_tbl`: A `tibble` is now recommended instead of a vector.

### Confidence Intervals
* `confint_betabinom.default()` (former `confint_betabinom()`): Renamed `event` with `status`. Renamed `loc_sc_params` with `dist_params`. Added argument `b_lives` which allows the user to specify probabilities `p` for `B_p-lives` to be considered.
* `confint_fisher.default()` (former `confint_fisher()`): Renamed `event` with `status`. Renamed `loc_sc_params` with `dist_params`. Renamed `loc_sc_varcov` with `dist_varcov`. Added argument `b_lives` which allows the user to specify probabilities `p` for `B_p-lives` to be considered.
* `delta_method()`: Renamed `loc_sc_params` with `dist_params`. Renamed `loc_sc_varcov` with `dist_varcov`.
* `plot_conf.default()` (former `plot_conf()`): Switched position of arguments `direction` and `distribution`.

### Monte Carlo Simulation
* `dist_delay_register()`: Deprecated, use `dist_delay()` instead.
* `dist_delay_report()`: Deprecated, use `dist_delay()` instead.
* `mcs_delay_register()`: Deprecated, use `mcs_delay()` instead. Renamed `x` with `time`. Renamed `event` with `status`. Removed `seed`. Removed `int_seed` from output list.
* `mcs_delay_report()`: Deprecated, use `mcs_delay()` instead. Renamed `x` with `time`. Renamed `event` with `status`. Removed `seed`. Removed `int_seed` from output list.
* `mcs_delays()`: Deprecated, use `mcs_delay()` instead. Renamed `x` with `time`. Renamed `event` with `status`. Removed `seed`. Removed `int_seed` from output list.
* `dist_mileage()`: Removed `event`. Renamed `x` with `time`. Switched position of arguments `time` and `mileage`.
* `mcs_mileage()`: Removed `event`. Renamed `x` with `time`. Switched position of arguments `time` and `mileage`.

## New Features
* Added support for ggplot2 in all plot functions. Plot method can be selected in `plot_prob()` or `plot_pop()` via argument `plot_method`.
* Added `reliability_data()`: Create consistent reliability data.
* Added `estimate_cdf()`: Unite functionality of `mr_method()`, `johnson_method()`, `kaplan_method()` and `nelson_method()`. Added option `ties.method` for `method = "mr"`, which specifies how ties should be treated.
* Support of multiple methods in `estimate_cdf()` and all functions that depend on the `cdf_estimation` (`rank_regression()`, `plot_prob()`, `plot_mod()`, `plot_conf()`, `mixmod_regression()`).
* `plot_prob()` and `plot_mod()` are able to handle mixture models.
* `mixmod_regression()` is now more flexible. Argument `k` can be used to control number of subgroups or to determine them in an automatic fashion. Argument `control` provides additional control over the segmentation procedure.
* Added `print.wt_rank_regression()`, `print.wt_ml_estimation()`, `print.wt_model_estimation()`, `print.wt_model_estimation_list()`, `print.wt_mixmod_regression()` and `print.wt_mixmod_regression_list()`.
* Added `vcov.wt_model_estimation()`.
* Added `dist_delay()`: Generalizes the distribution-specific modeling of delays. 
* Added `mcs_delay()`: Generalizes the adjustment of operating times by delays and supports multiple delays at once.
* Added lifecycle badges

## Introduction of S3 interface
* `rank_regression()` is now an S3 generic. `rank_regression()` becomes `rank_regression.default()`. Added `rank_regression.wt_cdf_estimation()`.
* `plot_prob()` is now an S3 generic. `plot_prob()` becomes `plot_prob.default()`. Added `plot_prob.wt_cdf_estimation()` and `plot_prob.wt_model()`.
* `plot_mod()` is now an S3 generic. `plot_mod()` becomes `plot_mod.default()`. Added `plot_mod.wt_model()`.
* `plot_conf()` is now an S3 generic. `plot_conf()` becomes `plot_conf.default()`. Added `plot_conf.wt_confint()`.
* `plot_pop()`: Added support for multiple population lines and comparison of two- and three-parametric distributions.

## Documentation improvements
* Revised README.
* Revised vignettes.
* Capitalized parameter documentation.

## Lifecycle changes

### Deprecated
* `dist_delay_register()` and `dist_delay_report()`: Use `dist_delay()` instead. 
* `mcs_delay_register()`, `mcs_delay_report()` and `mcs_delays()`: Use `mcs_delay()` instead.
* `mr_method()`, `johnson_method()`, `kaplan_method()` and `nelson_method()`: Use `estimate_cdf()` instead.
* `plot_prob_mix()`: Use `plot_prob()` instead.
* `plot_mod_mix()`: Use `plot_mod()` instead.

### Removed
* `calculate_ranks`.
* `mixture_em_cpp`.
* `plot_layout`.

## Minor improvements and bug fixes
* Fixed bug inside `plot_mod_mix()` for the case of no mixture distribution.
* Fixed bug inside `confint_betabinom()`: many cases near one -> `unique()`.
* Fixed bug inside `mr_method()`: assigning a rank for the same lifetime. 
* Fixed bug inside `mixmod_regression`: call to `segmented::segmented.lm()` was incorrect.
* Added trace type `"scatter"` and scatter mode `"markers"` to plotly plots.
* `delta_method()`, `r_squared_profiling()` and `loglik_profiling()` were vectorized.

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
* New method for the computation of Fisher's Confidence Bounds regarding probabilities is used. These method is called "z-Procedure" and is more appropriate to manage the bend-back behavior. Therefore an adjustment of functions `delta_method()` and `confint_fisher()` was made. 
* Implementation of log-location-scale models with threshold parameter like three-parametric Weibull ("weibull3"), three-parametric lognormal ("lognormal3") and three-parametric loglogistic ("loglogistic3"). 
* Implementation of location-scale models like smallest extreme value ("sev"), normal ("normal") and logistic ("logistic"). 
* Implementation of _Log-Likelihood Profiling_ for three-parametric models in function `loglik_profiling()`. In general this function is used inside `ml_estimation()` for the purpose of estimating threshold parameter of three-parametric models. 
* Implementation of _R-Squared Profiling_ for three-parametric models in function `r_squared_profiling()`. In general this function is used inside `rank_regression()` for the purpose of estimating threshold parameter of three-parametric models.
* Implementation of _Log-Likelihood Function_ for all implemented models in function `loglik_function()`. In general this function is used inside `ml_estimation()` for the purpose of estimating the variance-covariance matrix of location-scale models "sev", "normal" and "logistic". The function is also used to estimate the variance-covariance matrix of log-location-scale models with a threshold parameter, i.e. "weibull3", "lognormal3" and "loglogistic3".
* new argument in function `ml_estimation()`: `wts` for case weights. 
