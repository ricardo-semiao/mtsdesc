# Helper functions used between more than one method:
acf_helpers <- env()

acf_helpers$format <- function(x, series, lag.max, type, na.action, demean) {
  lag.max <- lag.max %||% ceiling(10 * log(nrow(x) / ncol(x), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    purrr::imap_dfr(function(col, colname) {
      tibble::tibble(
        serie = colname,
        value = stats::acf(col, lag.max, type,
          plot = FALSE, na.action, demean
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


#' @noRd
test_acf <- function(x, series, lag.max, type, graph_type, ci,
  env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$category(graph_type, c("segment", "area"), env)
  test$interval(ci, 0, 1, FALSE, env)
  
  test$category(type, c("correlation", "covariance", "partial"), env)
  test$type(lag.max, c("NULL", "integer", "double"), env)
}

#'@noRd
setup_acf <- function(x, series, lag.max, type, na.action, demean, ...) {
  UseMethod("setup_acf")
}


#' Plot autocorrelation (and similars) of dataset
#'
#' \code{ggvar_acf} plots the auto-correlations (and similars) call for every
#'  series, using \link[ggplot2]{facet_wrap}. \code{ggvar_ccf} plots all the
#'  cross-correlations (and similars) between the series, using
#'  \link[ggplot2]{facet_grid}.
#'
#' @param x A dataset (object coercible to data.frame) or a "varest" object to
#'  get residuals from.
#' @eval param_series()
#' @param lag.max The number of lags used to calculate the ACF, passed to
#'  \link[stats]{acf}.
#' @param type The type of ACF to be computed. Can be either "correlation",
#'  "covariance", or "partial". Passed to \link[stats]{acf}.
#' @eval param_graph_type(c("segment", "area"))
#' @eval param_args_geom()
#' @eval param_args(c("geom_ribbon", "geom_hline", "facet_wrap"))
#' @param ci The level of confidence for the ACF confidence interval. Set to
#'  \code{FALSE} to omit the \link[ggplot2]{geom_ribbon}.
#' @eval param_facet()
#' @param na.action Function to be called to handle missing values.
#'  \code{na.pass} can be used.Passed to \link[stats]{acf}.
#' @param demean Logical. Should the covariances be about the sample means?
#'  Passed to \link[stats]{acf}.
#' @eval param_dots(c("setup_acf", "setup_ccf"))
#'
#' @return An object of class \code{ggplot}.
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
    args_geom = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list(),
    ci = 0.95,
    na.action = stats::na.fail, demean = TRUE,
    ...) {
  test_acf(x, series, lag.max, type, graph_type, ci)

  setup <- setup_acf(x, series, lag.max, type, na.action, demean, ...)

  graph_add <- inject(list(
    if (graph_type == "segment") {
      ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), !!!args_geom)
    } else if (graph_type == "area") {
      ggplot2::geom_area(aes(y = .data$value), !!!args_geom)
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




#' @noRd
setup_acf.varest <- function(x, series, lag.max, type, na.action, demean,
  ...) {
  x <- as.data.frame(stats::residuals(x))

  series <- series %||% colnames(x)
  title <- paste(acf_helpers$title_base(type), "Series")

  data <- acf_helpers$format(x, series, lag.max, type, na.action, demean)

  list(data = data, title = title)
}

#' @noRd
setup_acf.default <- function(x, series, lag.max, type, na.action, demean,
  ...) {
  x <- as.data.frame(x) %>% ignore_cols()

  series <- series %||% colnames(x)
  title <- paste(acf_helpers$title_base(type), "VAR Residuals")

  data <- acf_helpers$format(x, series, lag.max, type, na.action, demean)

  list(data = data, title = title)
}
