
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weibulltools

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/weibulltools)](https://CRAN.R-project.org/package=weibulltools)
<!-- badges: end -->

Unlike other R packages for survival analysis, the weibulltools package
focuses on reliability methods and has the advantage of being open
source and easily accessible contrary to other reliability analysis
software. In addition to that the package can be integrated into
(partly) automated data analysis processes and even can be connected to
big data systems.

The weibulltools package contains methods for examining bench test or
field data using the well-known weibull analysis. It includes Monte
Carlo simulation for estimating the life span of products that have not
failed yet, taking account of registering and reporting delays. On this
basis, if the products looked upon are vehicles, the covered mileage can
be estimated as well. The weibulltools package also provides methods for
probability estimation within samples that contain failures as well as
censored data. Methods for estimating the parameters of lifetime
distributions as well as the confidence intervals of quantiles and
probabilities are also included. If desired, the data can automatically
be divided into subgroups using segmented regression. And if the number
of subgroups in a Weibull Mixture Model is known, data can be analyzed
using the EM-Algorithm. Besides the calculation methods, methods for
interactive visualization of the edited data using *Plotly* are provided
as well.

## Installation

You can install the released version of weibulltools from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("weibulltools")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Tim-TU/weibulltools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(weibulltools)
## basic example code
```
