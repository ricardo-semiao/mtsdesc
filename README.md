
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/varr)](https://CRAN.R-project.org/package=varr)
[![R-CMD-check](https://github.com/ricardo-semiao/varr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardo-semiao/varr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The varr R package is being developed to provide general utility
functions for projects involving Vector Auto-Regressive models, with
specifically the [vars
package](https://cran.r-project.org/web/packages/vars/index.html).

**Disclaimer:** this package is in the early most stage of life. It
hasn’t been thoroughly tested and can present several bugs. I don’t
recommend using it for large-scale projects, yet.

Please report any problems in my email (below) or as a github issue.
Thank you!

Author: Ricardo Semião e Castro (ricardo.semiao@outlook).

## Introduction

Mainly, the functions retrieve results from *vars* objects, manipulate
them with [tidyverse](https://www.tidyverse.org/), and plot commonly
needed ggplot graphs, in the realm of IRF’s, FEVD’s, predict graphs, VAR
series and residual analysis.

In the future, I intent to add functions for quick (not thorough)
analysis and retrieval of trends, seasonality, and structural breaks,
using common methods and statistical tests. Additionally, I want to
expand the package to other VAR-like objects.

## Installation

You can install the development version of varr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ricardo-semiao/varr")
```

## Upcoming changes

- Start using rlang and glue APIs wherever possible.
  - Specially in the “setup” suite, as in the
    [vardiff](https://ricardo-semiao.github.io/vardiff/) package.
- More thorough tests:
  - Specify hat errors to expect in `expect_error` calls.
  - Directly test for elements of the ggplots returned by functions.
  - More tests specifically for `test_fun()`.
  - Add tests for “helper” and “setup” functions.
  - Rethink the `test` functions list scheme.
- Rethink some naming conventions:
  - Change “args\_…” to “…\_args” in arguments names
  - Change “serie(s)” naming to “var(s)” or “col(s)”
  - Change names of internal faceting variables
- Give more info in help pages:
  - Give hints on how to work with results.
  - Direct to used methods like “see: ?vars:::predict”.

Additional:

- Add package data and custom examples
- Solve some less important lintr notes.
- Study importing whole rlang and ggplot2.

## Additional information

This package:

- Follows the [tydiverse style guide](https://style.tidyverse.org/).
  - Using the [styler](https://styler.r-lib.org/) and
    [lintr](https://lintr.r-lib.org/) packages for style consistency.
- Uses [testthat](https://testthat.r-lib.org/) and
  [vdiffr](https://vdiffr.r-lib.org/) for automate tests.
- Uses [rlang](https://rlang.r-lib.org/) frameworks for *tidy eval* and
  *rlang errors*.
