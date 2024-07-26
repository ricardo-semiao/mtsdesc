# Helper functions used between more than one method:
acf_helpers <- list()

acf_helpers$format <- function(x, series, lag.max, type, ...) {
  lag.max <- lag.max %||% ceiling(10 * log(nrow(x) / ncol(x), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    purrr::imap_dfr(function(col, colname) {
      tibble::tibble(
        serie = colname,
        value = stats::acf(col, lag.max, type,
          plot = FALSE, ...
        )$acf[, , 1],
        lag = lag.min:lag.max
      )
    })
}

acf_helpers$title_base <- function(type) {
  switch(type,
    "correlation" = "Auto-correlation of",
    "covariance" = "Auto-covariance of",
    "partial" = "Auto-partial-correlation of"
  )
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
acf_test <- function(x, series, lag.max, type, graph_type, ci,
  env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$category(graph_type, c("segment", "area"), env)
  test$interval(ci, 0, 1, FALSE, env)
  
  test$category(type, c("correlation", "covariance", "partial"), env)
  test$type(lag.max, c("NULL", "integer", "double"), env)
}

#'@noRd
acf_setup <- function(x, series, lag.max, type, ..., env = caller_env()) {
  UseMethod("acf_setup")
}


#' Plot autocorrelation and similar
#'
#' `ggvar_acf` plots the auto-correlations (and similar) call for every
#' series. `ggvar_ccf` plots all the cross-correlations (and similar) between
#' the series, in a grid.
#'
#' @param x A dataset (object coercible to data.frame) or a "varest" object to
#'  get residuals from.
#' @eval roxy$series()
#' @param lag.max The number of lags used to calculate the ACF, passed to
#'  [acf][stats::acf].
#' @param type The type of ACF to be computed. Can be either "correlation",
#'  "covariance", or "partial". Passed to [acf][stats::acf].
#' @eval roxy$graph_type(c("segment", "area"))
#' @eval roxy$args_type()
#' @eval roxy$args_gg(c("geom_ribbon", "geom_hline", "facet_wrap"))
#' @eval roxy$ci("ggplot2::geom_ribbon")
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom()`
#' `r roxy$details_methods()$acf`
#' 
#' @eval roxy$return_gg()
#' 
#' @eval roxy$fam_ts()
#' @eval roxy$fam_diag()
#'
#' @examples
#' ggvar_acf(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_ccf(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_acf(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_acf <- function(
    x, series = NULL,
    lag.max = NULL, type = "correlation",
    graph_type = "segment",
    args_type = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list(),
    ci = 0.95,
    ...) {
  acf_test(x, series, lag.max, type, graph_type, ci)

  setup <- acf_setup(x, series, lag.max, type, ...)

  graph_add <- inject(list(
    if (graph_type == "segment") {
      ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), !!!args_type)
    } else if (graph_type == "area") {
      ggplot2::geom_area(aes(y = .data$value), !!!args_type)
    },
    if (!is_false(ci)) {
      dist <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(setup$data))
      ggplot2::geom_ribbon(aes(ymin = -dist, ymax = dist), !!!args_ribbon)
    }
  ))

  inject(
    ggplot(setup$data, aes(.data$lag, .data$value)) +
      graph_add +
      ggplot2::geom_hline(yintercept = 0, !!!args_hline) +
      ggplot2::facet_wrap(vars(.data$serie), !!!args_facet) +
      ggplot2::labs(title = setup$title, x = "Lags", y = "Values")
  )
}


# Setup methods:
#' @noRd
acf_setup.varest <- function(x, series, lag.max, type, ..., env) {
  x <- as.data.frame(stats::residuals(x))

  series <- get_series(series, colnames(x), env)
  title <- paste(acf_helpers$title_base(type), "Series")

  data <- acf_helpers$format(x, series, lag.max, type, ...)

  list(data = data, title = title)
}

#' @noRd
acf_setup.default <- function(x, series, lag.max, type, ..., env) {
  x <- as.data.frame(x) %>% ignore_cols()

  series <- get_series(series, colnames(x), env)
  title <- paste(acf_helpers$title_base(type), "VAR Residuals")

  data <- acf_helpers$format(x, series, lag.max, type, ...)

  list(data = data, title = title)
}
