---
title: "Life Data Analysis Part I - Estimation of Failure Probabilities"
subtitle: "A Non-parametric Approach"
author: "Tim-Gunnar Hensel"
date: "2020-12-15"
output:
  rmarkdown::html_vignette:
    fig_height: 6
    fig_width: 7
    fig_caption: yes
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Life Data Analysis Part I - Estimation of Failure Probabilities}
  %\VignetteEncoding{UTF-8}
---



This document presents non-parametric methods for estimating the failure probabilities of units and their presentation in interactive visualizations. A unit can be a single component, an assembly or an entire system. 

## Introduction to Life Data Analysis

If the lifetime of a unit is considered to be a continuous random variable _T_, then the probability that a unit has failed by a certain point in time or a distance _t_ is defined by its CDF _(cumulative distribution function)_ _F(t)_.

$$ P(T\leq t) = F(t) $$

In order to obtain an estimate of the cumulative failure probability for each observation $t_1, t_2, ..., t_n$ two approaches are possible. Using a parametric lifetime distribution requires that the underlying assumptions for the sample data are valid. If the distribution-specific assumptions are correct, the model parameters can be estimated and the CDF is computable. But if the required conditions could not be met, interpretations and derived conclusions are not reliable. A more general approach for the calculation of the cumulative failure probability is to use non-parametric statistical estimators $\hat{F}(t_1), \hat{F}(t_2), ..., \hat{F}(t_n)$. In comparison to a parametric distribution no general assumptions must be held. For non-parametric estimators, an ordered sample of size n is needed. Starting at 1, the ranks $i \in \{1, 2, ..., n \}$ are assigned to the ascending sorted sample values. Since there is a known relationship between ranks and corresponding ranking probabilities a CDF can be calculated.  

But rank distributions are systematically skewed distributions and thus the median value instead of the expected value $E\left[F\left(t_i\right)\right] = \frac{i}{n + 1}$ is used for estimation [^note1]. This skewness is visualized in _Figure 1_. 


```r
library(tidyverse) # using dplyr manipulation functions and ggplot2
#> Warning: Paket 'tibble' wurde unter R Version 4.0.3 erstellt

x <- seq(0, 1, length.out = 100) # CDF
n <- 10 # sample size
i <- c(1, 3, 5, 7, 9) # ranks
r <- n - i + 1 # inverse ranking

df_dens <- expand.grid(cdf = x, i = i) %>% 
  mutate(n = n, r = n - i + 1, pdf = dbeta(x = x, shape1 = i, shape2 = r))

densplot <- ggplot(data = df_dens, aes(x = cdf, y = pdf, colour = as.factor(i))) + 
  geom_line() + 
  scale_colour_discrete(guide = guide_legend(title = "i")) + 
  theme_bw() + 
  labs(x = "Failure Probability", y = "Density")
densplot
```

![Figure 1: Densities for different ranks i in samples of size n = 10.](figure/rank densities-1.png)

[^note1]: Kapur, K. C.; Lamberson, L. R.: _Reliability in Engineering Design_, 
          _New York: Wiley_, 1977, pp. 297-301  

### Failure Probability Estimation  

In practice, a simplification for the calculation of the median value, also called median rank, is made. The formula of _Benard's Approximation_ is given by 

$$\hat{F}(t_i) \approx \frac{i - 0,3}{n + 0,4} $$ 

and is described in _The Plotting of Observations on Probability Paper _ [^note2]. 

[^note2]: Benard, A.; Bos-Levenbach, E. C.: _The Plotting of Observations on Probability Paper_, 
          _Statistica Neerlandica 7 (3)_, 1953, pp. 163-173  
          
However, this equation only provides valid estimates for failure probabilities if all units in the sample are defectives (`estimate_cdf(methods = "mr", ...)`).

In field data analysis, however, the sample mainly consists of intact units and only a small fraction of units failed. Units that have no damage at the point of analysis and also have not reached the operating time or mileage of units that have already failed, are potential candidates for future failures.  As these, for example, still are likely to fail during a specific time span, like the guarantee period, the failure probability must be adjusted upwards by these potential candidates.  

A commonly used method for correcting probabilities of (multiple) right censored data is Johnson's method (`estimate_cdf(methods = "johnson", ...)`). By this method, all units that fall into the period looked at are sorted in an ascending order of their operating time or mileage. If there are units that have not failed before the _i_-th failure, an adjusted rank for the _i_-th failure is formed. This correction takes the potential candidates into account and increases the rank number. In consequence, a higher rank leads to a higher failure probability. This can be seen in _Figure 1_.
  
The rank adjustment is calculated as follows: 

$$j_i = j_{i-1} + x_i \cdot I_i, \;\; with \;\; j_0 = 0$$

Here, $j_ {i-1}$ is the adjusted rank of the previous failure, $x_i$ is the number of defectives at time/distance $t_i$ and $I_i$ is the increment that corrects the rank by the candidates. 

$$I_i=\frac{(n+1)-j_{i-1}}{1+(n-n_i)}$$

The sample size is $n$ and $n_i$ is the number of units that have a lower operating time/mileage than the _i_-th unit. Once the adjusted ranks are calculated, the failure probabilities can be estimated according to _Benard's Approximation_.  

Other methods in `weibulltools` that can also handle (multiple) right censored data are the Kaplan-Meier estimator (`estimate_cdf(methods = "kaplan")`) and the Nelson-Aalen estimator (`estimate_cdf(method = "nelson")`). 

### Probability Plotting  

After computing failure probabilities a method called _Probability Plotting_ is applicable. It is a graphical _goodness of fit_ technique that is used in assessing whether an assumed distribution is appropriate to model the sample data.  

The axes of a probability plot are transformed in such a way that the CDF of a specified model is represented through a straight line. If the plotted points (`plot_prob()`) fall on an approximately straight line it can be said that the chosen distribution is adequate.  

The two-parameter Weibull distribution can be parameterized with $\eta$ and $\beta$ such that the CDF is characterized by the following equation:  

$$F(t)=1-\exp\left[ -\left(\frac{t}{\eta}\right)^{\beta}\right]$$

Then a linearized version of the CDF is: 

$$ \log\left[-\log(1-F(t))\right] = \beta \cdot \log(t) - \beta \cdot \log(\eta)$$

This leads to the following transformations regarding the axes: 

* Abscissa: $x = \log(t)$ 
* Ordinate: $y = \log\left[-\log(1-F(t))\right]$.

Another version of the Weibull CDF such that the distribution is part of the log-location-scale family with parameters $\mu$ and $\sigma$ is:  

$$F(t)=\Phi_{SEV}\left(\frac{\log(t) - \mu}{\sigma}\right)$$

A linearized representation of this CDF is: 

$$\Phi^{-1}_{SEV}\left(F(t)\right)=\frac{1}{\sigma} \cdot \log(t) - \frac{\mu}{\sigma}$$

This leads to the following transformations regarding the axes: 

* Abscissa: $x = \log(t)$ 
* Ordinate: $y = \Phi^{-1}_{SEV}\left(F(t)\right)$, which is the quantile function of the 
  SEV (_smallest extreme value_) distribution.  
  
It can be easily seen that the parameters can be converted into each other. The corresponding equations are: 

$$\beta = \frac{1}{\sigma} \;\; and $$  

$$\eta = \exp\left(\mu\right).$$   

## Data: shock

To apply the introduced methods of non-parametric failure probability estimation and probability plotting the `shock` data is used. In this dataset kilometer-dependent problems that have occurred on shock absorbers are reported. In addition to failed items the dataset also contains non-defectives, so called *censored* observations. The data can be found in _Statistical Methods for Reliability Data_ [^note3]. 

[^note3]: Meeker, W. Q.; Escobar, L. A.: _Statistical Methods for Reliability Data_, 
          _New York, Wiley series in probability and statistics_, 1998, p. 630









