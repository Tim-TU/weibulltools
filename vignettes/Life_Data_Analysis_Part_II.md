---
title: "Life Data Analysis Part II - Parameter Estimation of Parametric Lifetime Models"
subtitle: "Median Rank Regression and Maximum Likelihood Method"
author: "Tim-Gunnar Hensel"
date: "2020-12-15"
output:
  rmarkdown::html_vignette:
    fig_height: 6
    fig_width: 7
    fig_caption: yes
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Life Data Analysis Part II - Parameter Estimation of Parametric Lifetime Models}
  %\VignetteEncoding{UTF-8}
---



This document introduces two methods for the parameter estimation of lifetime models. Where Median Rank Regression _(MRR)_ fits a straight line through transformed plotting positions (transformation is described precisely in `vignette(topic = "Life_Data_Analysis_Part_I", package = "weibulltools")`), Maximum Likelihood _(ML)_ strives to maximize a function of the parameters given the sample data. If the parameters are obtained a cumulative distribution function _(CDF)_ can be computed and added to a probability plot.  

In the theoretical part of this vignette we will focus on a two-parameter Weibull distribution. The second part is about the application of the provided estimation methods in `weibulltools`. All implemented models can be found in the help pages of `rank_regression()` and `ml_estimation()`.

## The Weibull Distribution 

The Weibull distribution is a continuous probability distribution, which is specified by the shape parameter $\beta$ and the scale parameter $\eta$ (two-parameter representation). Its CDF and PDF _(probability density function)_ are given by the following formula: 

$$F(t)=1-\exp\left[ -\left(\frac{t}{\eta}\right)^{\beta}\right]$$

$$f(t)=\frac{\beta}{\eta} \cdot \left(\frac{t}{\eta}\right)^{\beta-1} \cdot \exp\left[-\left(\frac{t}{\eta}\right)^\beta\right]$$
The practical benefit of the Weibull in the field of lifetime analysis is that the common profiles of failure rates, which are observed over the lifetime of a large number of technical products, can be described using this statistical distribution.

In the following, the estimation of the specific parameters $\beta$ and $\eta$ is explained.  

## Median Rank Regression (MRR) 

In MRR the cumulative distribution function is linearized so that the true, unknown population is estimated by a straight line which is analytically placed among the plotting pairs. The lifetime characteristic, entered on the x-axis, is displayed on a logarithmic scale. A double-logarithmic representation of the estimated failure probabilities is used for the y-axis. Using Ordinary Least Squares _(OLS)_ we will determine a best-fit line in order that the sum of squared deviations between this fitted regression line and the plotted points is minimized.  

In reliability analysis, it became prevalent that the line is placed in the probability plot so that the horizontal distances between the best-fit line and the points are minimized [^note1]. This procedure is called __x on y__ rank regression.  

[^note1]: Berkson, J.: _Are There Two Regressions?_, 
          _Journal of the American Statistical Association 45 (250)_, 
          DOI: 10.2307/2280676, 1950, pp. 164-180  
          
The formulas for estimating the slope and the intercept of the regression line according to the described method are given below.  

Slope: 

$$\hat{b}=\frac{\sum_{i=1}^{n}(x_i-\bar{x})\cdot(y_i-\bar{y})}{\sum_{i=1}^{n}(y_i-\bar{y})^2}$$  

Intercept:  

$$\hat{a}=\bar{x}-\hat{b}\cdot\bar{y}$$  

With  

$$x_i=\log(t_i)\;;\; \bar{x}=\frac{1}{n}\cdot\sum_{i=1}^{n}\log(t_i)\;;$$  

as well as  

$$y_i=\log\left[-\log(1-F(t_i))\right]\;and \; \bar{y}=\frac{1}{n}\cdot\sum_{i=1}^{n}\log\left[-\log(1-F(t_i))\right].$$  

In order to obtain the Weibull-specific parameters the slope and the intercept needs to be transformed [^note2].  

$$\hat{\beta}=\frac{1}{\hat{b}}$$  

$$\hat{\eta}=\exp(\hat{a})$$
Using the location-scale parameterization, which is mentioned in `vignette(topic = "Life_Data_Analysis_Part_I", package = "weibulltools")`, we can see that $b$ equals $\sigma$ and $a$ equals $\mu$.  

[^note2]: ReliaSoft Corporation: _Life Data Analysis Reference Book_, 
          online: [ReliaSoft](http://reliawiki.org/index.php/The_Weibull_Distribution), accessed 09 January 2018  
          
## Maximum Likelihood (ML)

The ML method of Ronald A. Fisher estimates the parameters by maximizing the likelihood function. Assuming a theoretical distribution, the idea of ML is that the specific parameters are chosen in such a way that the plausibility of obtaining the present sample is maximized. The log-likelihood is given by the following equation:  

$$\log L = n \cdot \log\left(\frac{\beta}{\eta}\right) - \sum_{i=1}^n\left(\frac{t_i}{\eta}\right)^\beta + \left(\beta - 1\right) \cdot \sum_{i = 1}^n\log\left(\frac{t_i}{\eta}\right)$$  

Deriving and nullifying the log-likelihood function according to $\beta$ results in:

$$\frac{\partial \log L}{\partial \beta}=\frac{n}{\beta}+\sum_{i=1}^{n}\log(t_i)-\frac{n\cdot\sum_{i=1}^{n}(t_i^{\beta}\cdot \log(t_i))}{\sum_{i=1}^{n}(t_i)^\beta}=0$$  

Since there is no closed-form expression $\beta$ needs to be determined numerically. Once received, the parameter $\eta$ can be calculated analytically.

$$\hat{\eta}=[\frac{1}{n}\cdot\sum_{i=1}^{n}t_i^{\hat{\beta}}]^\frac{1}{\hat{\beta}}$$  
 
In large samples, ML estimators have optimality properties. In addition, the simulation studies by Genschel and Meeker [^note3] have shown that even in small samples it is difficult to find an estimator that regularly has better properties 
than ML estimators.

[^note3]: Genschel, U.; Meeker, W. Q.: _A Comparison of Maximum Likelihood and Median-Rank Regression for Weibull Estimation_, 
          in: _Quality Engineering 22 (4)_, DOI: 10.1080/08982112.2010.503447, 2010, pp. 236-255


## Data I: shock

To apply the introduced estimation methods and related visualizations the `shock` data is used. In this dataset kilometer-dependent problems that occurred on shock absorbers are reported. In addition to failed items the 
dataset also contains non-defectives. The data can be found in _Statistical Methods for Reliability Data_ [^note4]. 

[^note4]: Meeker, W. Q.; Escobar, L. A.: _Statistical Methods for Reliability Data_, 
          _New York, Wiley series in probability and statistics_, 1998, p. 630  
          
## Median Rank Regression and Maximum Likelihood Estimation with Package `weibulltools`

Where `rank_regression()` determines a regression model (`x ~ y`) based on the linearized CDF of a specified model, `ml_estimation()` computes the parameters which maximize the log-likelihood of an underlying distribution. Both methods can be applied to complete failure data as well as failure and (multiple) right-censored data. A special characteristic of these functions is that the implemented distributions are parameterized in such a way that they are part of a (log-)location-scale family with parameters $\mu$ and $\sigma$. This representation is often used in reliability analysis. Subsequent transformations especially to get the Weibull-specific values in the scale-shape parameterization with $\eta$ and $\beta$ are included. This parameterization is also used in the `stats` package (e.g., in the function [`pweibull`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Weibull.html)). `rank_regression()` and `ml_estimation()` can also deal with models that have a threshold parameter $\gamma$ like the three-parametric Weibull. If a three-parametric distribution is specified both methods call corresponding profiling functions. Inside `rank_regression()` the function `r_squared_profiling()` is called and if `ml_estimation()` is used `loglik_profiling()` is executed. To get information about the provided confidence intervals of the parameters the help pages of `rank_regression()` and `ml_estimation()` should be read carefully.  

In the following we want to apply both methods to the dataset `shock`. We assume that the data can be best described by a two-parametric Weibull distribution. The estimated parameters will be used to calculate the population lines and for the purpose of comparison they will be visualized in one Weibull probability plot. 

### MRR-Code two-parametric Weibull


```r
data(shock)
# generate random ids for units: 
shock_data <- reliability_data(shock, x = distance, status = status)

# Rank Regression: 
# for rank_regression(), estimated failure probabilities are required: 
shock_cdf <- estimate_cdf(shock_data, methods = "johnson")

# Using all models which are provided in rank_regression: 
distributions <- c("weibull", "lognormal", "loglogistic", "normal", "logistic", "sev", 
           "weibull3", "lognormal3", "loglogistic3")

library(purrr)
r_sq_vec <- map_dbl(distributions, function(distribution) {
  rank_regression(shock_cdf, distribution = distribution)$r_squared
})

names(r_sq_vec) <- distributions

r_sq_vec
#>      weibull    lognormal  loglogistic       normal     logistic          sev 
#>    0.9901585    0.9641187    0.9820248    0.9846976    0.9720249    0.9563309 
#>     weibull3   lognormal3 loglogistic3 
#>    0.9901585    0.9641187    0.9820248
```
<br> 

The use of a two-parametric Weibull (`weibull`) is acceptable since $R^2$ is highest. It should also be noticed that $R^2$ values for `weibull`, `lognormal`, `loglogistic` are equal to their three-parametric models `weibull3`, `lognormal3`, `loglogistic3`. This means that the estimate of threshold parameter is 0.  

We will construct a probability plot for the Weibull distribution and add the estimated regression line. 


```r
# Again estimating weibull: 
mrr_weibull <- rank_regression(shock_cdf, distribution = "weibull")
mrr_weibull 
#> Rank Regression
#> Coefficients:
#>       eta       beta  
#> 28554.796      2.753

# Probability plot: 
weibull_grid <- plot_prob(
  shock_cdf,
  distribution = "weibull", 
  title_main = "Weibull Probability Plot", 
  title_x = "Mileage in km", 
  title_y = "Probability of Failure in %",
  title_trace = "Defect Shock Absorbers",
  plot_method = "ggplot2"
)

# Add regression line: 
weibull_plot <- weibull_grid %>% 
  plot_mod(
    mrr_weibull,
    title_trace = "Median Rank Regression"
  )

weibull_plot
```

![Figure 1: Median Rank Regression using two-parameter Weibull.](figure/MRR weibull-1.png)

### ML-Code two-parametric Weibull


```r
# Using all models which are provided in ml_estimation: 
ml_list <- map(distributions, function(distribution) {
  ml_estimation(shock_data, distribution = distribution)
})

loglik_vec <- map_dbl(ml_list, function(el) el$logL)
names(loglik_vec) <- distributions

loglik_vec
#>      weibull    lognormal  loglogistic       normal     logistic          sev 
#>    -123.9954    -124.6085    -124.3654    -124.2301    -124.5476    -124.6229 
#>     weibull3   lognormal3 loglogistic3 
#>    -123.9954    -124.6085    -124.3654
```

<br> 

We can see that the estimate for threshold parameter of the three-parametric distributions is 0, since the log-likelihood values of `weibull`, `lognormal`, `loglogistic` are equal to their three-parametric representations `weibull3`, `lognormal3`, `loglogistic3`. As the log-likelihood value of the two-parameter Weibull is highest, its usage is again justified. 

For comparison _Figure 1_ will be extended by a straight line estimated with ML. Thus, we use the `add_lines()` function which is part of package `plotly`. 


```r
# Again estimating weibull: 
ml_weibull <- ml_estimation(
  shock_data, 
  distribution = "weibull"
)

ml_weibull 
#> Maximum Likelihood Estimation
#> Coefficients:
#>      eta      beta  
#> 27718.71      3.16

# Add ML estimation to weibull_plot: 
weibull_both <- weibull_plot %>%
  plot_mod(ml_weibull, title_trace = "Maximum Likelihood")

weibull_both
```

![Figure 2: Comparison of Median Rank Regression and Maximum Likelihood.](figure/ML weibull-1.png)

## Data II: Alloy T7989

Finally we will use the dataset `Alloy T7989` in which the cycles until a fatigue failure of a special alloy occurs are inspected. The data is also taken from Meeker and Escobar [^note5]. The authors visualized the data in a Weibull and Log-normal probability plot and detected a right-curved pattern. Thus, two-parametric distributions won't be adequate and they fitted three-parametric models. In the following we want to compare the fit of a two- and three-parametric Log-normal 
using method `ml_estimation()`.  

[^note5]: Meeker, W. Q.; Escobar, L. A.: _Statistical Methods for Reliability Data_, 
          _New York, Wiley series in probability and statistics_, 1998, p. 131  

### ML-Code two- and three-parametric Log-normal


```r
# Data: 
cycles <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
            224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
            180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
            159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
            139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
            112, 108, 104, 99, 99, 96, 94)
state <- c(rep(0, 5), rep(1, 67))
id <- 1:length(cycles)

cycles_data <- reliability_data(x = cycles, status = state, id = id)

# Two-parameter Log-normal:  
ml_lognormal <- ml_estimation(
  cycles_data,
  distribution = "lognormal"
)

ml_lognormal
#> Maximum Likelihood Estimation
#> Coefficients:
#>     mu   sigma  
#> 5.1278  0.3276

# Three-parameter Log-normal:  
ml_lognormal3 <- ml_estimation(
  cycles_data,
  distribution = "lognormal3"
)

ml_lognormal3
#> Maximum Likelihood Estimation
#> Coefficients:
#>      mu    sigma    gamma  
#>  4.5015   0.6132  72.0727
```

<br> 

The two model selection criteria `aic` and `bic` are smaller for `lognormal3` meaning
that this model is preferable.  


```r
# Constructing probability plot: 
johnson_cdf_tbl <- estimate_cdf(cycles_data, "johnson")

lognormal_grid <- plot_prob(
  johnson_cdf_tbl,
  distribution = "lognormal", 
  title_main = "Log-normal Probability Plot", 
  title_x = "Cycles", 
  title_y = "Probability of Failure in %",
  title_trace = "Failed Units",
  plot_method = "ggplot2"
)

# Add three-parametric model to grid: 
lognormal_plot <- lognormal_grid %>% 
  plot_mod(
    ml_lognormal3,
    title_trace = "Three-parametric Log-normal"
  )

lognormal_plot
```

![Figure 3: Three-parametric Log-normal distribution.](figure/ML visualization I-1.png)


```r
# Add two-parametric model to lognormal_plot: 
lognormal_both <- lognormal_plot %>%
  plot_mod(ml_lognormal, title_trace = "Two-parametric Log-normal")

lognormal_both
```

![Figure 4: Comparison of two- and three-parametric Log-normal distribution.](figure/ML visualization II-1.png)

<br> 

In _Figure 4_ we can see that the data is better described if the three-parametric model is used. 