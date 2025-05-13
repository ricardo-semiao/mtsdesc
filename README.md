
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mtsdesc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/mtsdesc)](https://CRAN.R-project.org/package=mtsdesc)
[![R-CMD-check](https://github.com/ricardo-semiao/mtsdesc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardo-semiao/mtsdesc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The mtsdesc R package is being developed to provide mtsdesc provides
commonly needed utilities when working with a set of time series,
i.e. multi-panel plots and multi-column summary- and testing-tables. The
package also has methods to work with residuals of fitter values of
multivariate time series models.

**Disclaimer:** this package is in the early most stage of life. It
hasn’t been thoroughly tested and can present several bugs. I don’t
recommend using it for large-scale projects, yet.

Please report any problems as a GitHub
[issue](https://github.com/ricardo-semiao/morphdown/issues). Other
comments can be posted as a GitHub
[discussion](https://github.com/ricardo-semiao/morphdown/discussions),
or sent in my email below. Thank you!

Author: Ricardo Semião e Castro (<ricardo.semiao@outlook>).

## Introduction

The main package functions are either plotting ones `ggmts_*()` or
summary-table ones `tbmts_*()`. The workflow of both is similar:

- They receive either a set of time series or a multivariate time series
  model.
- Then, the data is extracted. For the former, the series, for the
  latter, residuals, fitted, or predicted values. This is done with the
  internal generic functions `*_setup()`.
- Then, the data is processed into a plot or table. The output can be
  customized with `args_*` arguments, that inject options to each
  dimension of the plot or table. The result can be further processed by
  hand after the function call.

As of now, the only format of time series accepted is a data.frame-ish
object, and for model, only `varest` from the
[vars](https://cran.r-project.org/web/packages/vars/index.html) package.
But, the separation of the data extraction in `*_setup()` and the output
generation means that you can provide your custom methods for
`*_setup()` (eg. `*_setup.mts()`) and the rest of the function should
work. I intent to do a tutorial guide on how to do these extensions in
the future.

Additionally, the package was originally focused on VAR models, such
that the plotting functions are still called `ggmts_*()`, and there is
not yet table functions `tbmts_*()`.

## Installation

You can install the development version of mtsdesc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ricardo-semiao/mtsdesc")
```

## Development Comments

The package has some nice features, with already some testing and
documentation. But, I’ve recently changed its purpose to be more general
on mts, and adopted new testing conventions. Thus, it will go through a
major refactor in the next months.

Some of the more important features I plan to add in the future are:

- Major refactor:
  - Start using rlang and glue APIs wherever possible, especially in the
    “setup” suite.
  - Add tidyselect capabilities.
  - Choose a better convention for faceting variable names and
    “serie(s)”, “var(s)” or “col(s)” namings.
  - Add `vars` to suggests. Add `check_installed` for suggests.
  - Have better default methods for `*_setup()` functions (assuming
    dataframes).
  - Rethink the `test` functions list scheme.
  - Add `*_setup()` documentation in the function docs, talking about
    existing methods and directing to their engines (“see:
    ?vars:::predict”).
  - Define a convention for layers orders.
  - Study importing whole rlang and ggplot2.
- New features:
  - Create the table functions `tbmts_*()` for summary and testing
    tables.
  - Add option to remove facets in graphics with a single serie.
  - Study which plots could have different versions like `_colored`.
- More thorough tests:
  - Add tests for “helper” and “setup” functions.
  - Add tests for `test` function itself, and also for documentation
    `roxy`.
  - Specify what errors to expect in `expect_error` calls.
  - Directly test for elements of the ggplots returned by functions.
  - Study including warnings if arguments in `args_*` aren’t used by the
    relevant function.
- More thorough documentation:
  - Annotate all conventions in a .md file.
  - More tips on how to customize results in the vignette and in the
    function docs.
  - Add better examples, add an example.qmd vignette.
  - Solve some less important lintr notes.

Note that this package:

- Follows the [tydiverse style guide](https://style.tidyverse.org/).
  - Using the [styler](https://styler.r-lib.org/) and
    [lintr](https://lintr.r-lib.org/) packages for style consistency.
- Uses [testthat](https://testthat.r-lib.org/) and
  [vdiffr](https://vdiffr.r-lib.org/) for automate tests.
- Uses [rlang](https://rlang.r-lib.org/) frameworks for *tidy eval* and
  *rlang errors*.
