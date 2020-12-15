---
title: "Life Data Analysis Part III - Mixture Models"
subtitle: "Segmented Regression and EM-Algorithm"
author: "Tim-Gunnar Hensel"
date: "2020-12-15"
output:
  rmarkdown::html_vignette:
    fig_height: 6
    fig_width: 7
    fig_caption: yes
vignette: >
  %\VignetteIndexEntry{Life Data Analysis Part III - Mixture Models}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



In this vignette two methods for the separation of mixture models are presented. A mixture model can be assumed, if the points in a probability plot show one or more changes in slope, depict one or several saddle points or follow an S-shape. A mixed distribution often represents the combination of multiple failure modes and thus must be splitted in its components to get reasonable results in further analyses. 

Segmented regression aims to detect breakpoints in the sample data from whom a split in subgroups can be made. The EM-Algorithm is a computation-intensive method that iteratively tries to maximize a likelihood function, which is weighted by the posterior probability, the conditional probability that an observation belongs to subgroup _k_.  

In the following we will focus on the application of these methods and their visualizations using functions `mixmod_regression()`, `mixmod_em()`, `plot_prob_mix()` and `plot_mod_mix()`, which are implemented in `weibulltools`. 

## Data: Voltage Stress Test

To apply the introduced methods we will use a dataset where units were passed to a high voltage stress test. _hours_ indicates the number of hours until a failure occurs, or the number of hours until a unit was taken out of the test and has  not failed. _state_ is a flag variable and describes the condition of a unit. If a unit failed the flag is 1 and 0 otherwise. Data was taken from _Reliability Analysis by Failure Mode_ [^note1]. 

[^note1]: Doganaksoy, N.; Hahn, G.; Meeker, W. Q.: _Reliability Analysis by Failure Mode_, 
          Quality Progress, 35(6), 47-52, 2002 

## Probability Plot for Voltage Stress Test Data

To get an intuition whether we can assume the presence of a mixture model, we will 
construct a Weibull probability plot. 


```r
# Data: 
data <- reliability_data(voltage, x = hours, status = status)

# Estimating failure probabilities: 
cdf_john_tbl <- estimate_cdf(data, "johnson")

# Probability plot: 
weibull_plot <- plot_prob(
  cdf_john_tbl,
  distribution = "weibull", 
  title_main = "Weibull Probability Plot", 
  title_x = "Time in Hours", 
  title_y = "Probability of Failure in %",
  title_trace = "Defect Items",
  plot_method = "ggplot2"
)

weibull_plot
```

![Figure 1: Plotting positions in weibull grid.](figure/probability plot weibull-1.png)

<br>
Since there is an obvious slope change in the Weibull probability plot of _Figure 1_, the appearance of a mixture model is strengthened.  

## Segmented Regression with Package `weibulltools`

In package `weibulltools` the method of segmented regression is implemented in function `mixmod_regression()`. If a breakpoint was detected, the failure data is separated by that point. After breakpoint detection the function `rank_regression()` is called inside `mixmod_regression()` and is used to estimate the distribution parameters of the subgroups. The visualization of the obtained results is done by functions `plot_prob_mix()` and `plot_mod_mix()`. The produced graph of `plot_prob_mix()` is pretty similar to the graph provided by `plot_prob()`, but the difference is, that the detected subgroups are colored differently. `plot_mod_mix()` then is used to add the estimated regression line of every sub-distribution. In the following the described procedure is expressed with code.  


```r
# Applying mixmod_regression(): 
mixreg_weib <- mixmod_regression(cdf_john_tbl, distribution = "weibull")

# Using plot_prob_mix(). 
mix_reg_plot <- plot_prob(
  mixreg_weib, 
  title_main = "Weibull Mixture Regression", 
  title_x = "Time in Hours", 
  title_y = "Probability of Failure", 
  title_trace = "Subgroup",
  plot_method = "ggplot2"
)

mix_reg_plot
```

![Figure 2: Subgroup-specific plotting positions.](figure/segmented weibull I-1.png)


```r
# Using plot_mod() to visualize regression lines of subgroups: 
mix_reg_lines <- plot_mod(
  mix_reg_plot, 
  mixreg_weib, 
  title_trace = "Fitted Line"
)

mix_reg_lines
```

![Figure 3: Subgroup-specific regression lines.](figure/segmented weibull II-1.png)

<br>
Without specifying the number of mixed components _(k)_ this method has splitted the data in two groups. This can bee seen in _Figure 2_ and _Figure 3_. To sum up, an upside of this function is that one does not have to specify the number of mixing components, since segmentation happens in an automated fashion. Nevertheless the intention of this function is to give a hint for the existence of a mixture model. An in-depth analysis should be done afterwards.  

## EM-Algorithm with Package `weibulltools`

The EM-Algorithm can be applied through the usage of the function `mixmod_em()`. In comparison to `mixmod_regression()` one has to specify _k_, the number of subgroups. The obtained results can be visualized by functions `plot_prob_mix()` and `plot_mod_mix()`, too.  



```r
# Applying mixmod_regression(): 
mix_em_weib <- mixmod_em(
  x = data, 
  distribution = "weibull",
  conf_level = 0.95, 
  k = 2, 
  method = "EM", 
  n_iter = 150
)

# Using plot_prob(): 
mix_em_plot <- plot_prob(
  mix_em_weib,
  title_main = "Weibull Mixture EM", 
  title_x = "Time in Hours", 
  title_y = "Probability of Failure", 
  title_trace = "Subgroup",
  plot_method = "ggplot2"
)

mix_em_plot
```

![Figure 4: Subgroup-specific plotting positions.](figure/em weibull I-1.png)


```r

# Using plot_mod() to visualize regression lines of subgroups: 
mix_em_lines <- plot_mod(mix_em_plot, mix_em_weib, title_trace = "Fitted Line")
#> Warning: Unknown or uninitialised column: `method`.

#> Warning: Unknown or uninitialised column: `method`.
mix_em_lines
```

![Figure 5: Subgroup-specific regression lines.](figure/em weibull II-1.png)

<br>
In comparison to `mixmod_regression()` the EM-Algorithm can also assign censored items to a specific subgroup. Hence, an individual analysis of the mixing components, depicted in _Figure 4_ and _Figure 5_, is possible. In conclusion an analysis of a mixture model using `mixmod_em()` is statistically founded. A drawback of this function is, that the identification of the number of subgroups can not be determined automatically.