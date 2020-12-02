
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weibulltools

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/weibulltools)](https://CRAN.R-project.org/package=weibulltools)
<!-- badges: end -->

## Overview

The *weibulltools* package focuses on statistical methods and
visualizations that are often used in reliability engineering.

The goal of *weibulltools* is to equip the user with a compact and
easily accessible set of methods and visualization tools that make the
examination and adjustment as well as the analysis and interpretation of
field data (and bench tests) as simple as possible.

Besides the well-known weibull analysis, *weibulltools* supports
multiple lifetime distributions and also contains Monte Carlo methods
for the correction and completion of imprecisely recorded or unknown
lifetime characteristics.  
Plots are created statically ([ggplot2](https://ggplot2.tidyverse.org/))
or interactively ([plotly](https://plotly.com/r/)) and can be customized
with the corresponding functions of the respective visualization
package.

## Installation

The latest released version of *weibulltools* from
[CRAN](https://cran.r-project.org/web/packages/weibulltools/) can be
installed with:

``` r
install.packages("weibulltools")
```

### Development version

Install the development version of *weibulltools* from
[GitHub](https://github.com/Tim-TU/weibulltools/) to use new features or
to get a bug fix.

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

## Getting help

If you notice a bug or have suggestions for improvements, please submit
an issue with a minimal reproducible example on
[GitHub](https://github.com/Tim-TU/weibulltools/issues). For further
questions, please contact [Tim-Gunnar
Hensel](mailto:tim-gunnar.hensel@tu-berlin.de).
