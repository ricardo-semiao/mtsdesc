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
#' @param type The type of ACF to be computed. Can be either "correlation",
#'  "covariance", or "partial". Passed to \link[stats]{acf}.
#' @param lag.max The number of lags used to calculate the ACF, passed to
#'  \link[stats]{acf}.
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
#' ggvar_ccf_grid(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_acf(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_acf <- function(
    x, series = NULL,
    type = "correlation", lag.max = NULL,
    graph_type = "segment",
    args_geom = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list(),
    ci = 0.95,
    na.action = na.fail, demean = TRUE,
    ...) {
  test_acf(x, series, type, lag.max, graph_type, ci)

  setup <- setup_acf(x, series, type, lag.max, na.action, demean, ...)

  graph_add <- inject(c(
    if (graph_type == "segment") {
      list(
        ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), !!!args_geom)
      )
    } else if (graph_type == "area") {
      list(
        ggplot2::geom_area(aes(y = .data$value), !!!args_geom)
      )
    },
    if (!is_false(ci)) {
      dist <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(x))
      list(
        ggplot2::geom_ribbon(aes(ymin = -dist, ymax = dist), !!!args_ribbon)
      )
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

#' @rdname ggvar_acf
#' @export
ggvar_ccf <- function(
    x, series = NULL,
    type = "correlation", lag.max = NULL,
    graph_type = "segment",
    args_geom = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list(),
    ci = 0.95, facet_type = "ggplot",
    na.action = na.fail,
    ...) {
  test_ccf(x, series, type, lag.max, graph_type, ci, facet_type)

  title <- switch(type,
    "correlation" = "Cross-correlation of Series",
    "covariance" = "Cross-covariance of Series"
  )

  setup <- setup_ccf(x, series, type, lag.max, na.action, ...)

  graph_add <- inject(c(
    if (graph_type == "segment") {
      list(
        ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), !!!args_geom)
      )
    } else if (graph_type == "area") {
      list(
        ggplot2::geom_area(aes(y = .data$value), !!!args_geom)
      )
    },
    if (!is_false(ci)) {
      dist <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(x))
      list(
        ggplot2::geom_ribbon(aes(ymin = -dist, ymax = dist), !!!args_ribbon)
      )
    },
    list(define_facet(facet_type, "facet_x", "facet_y", !!!args_facet))
  ))

  inject(
    ggplot(setup$data, aes(.data$lag, .data$value)) +
      graph_add +
      ggplot2::geom_hline(yintercept = 0, !!!args_hline) +
      ggplot2::labs(title = title, x = "Lags", y = "Values")
  )
}


#' @noRd
test_acf <- function(x, series, type, lag.max, graph_type, ci,
  env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$category(type, c("correlation", "covariance", "partial"), env)
  test$type(lag.max, c("NULL", "integer", "double"), env)
  test$category(graph_type, c("segment", "area"), env)
  test$interval(ci, 0, 1, FALSE, env)
}

#'@noRd
setup_acf <- function(x, series, type, lag.max, na.action, demean, ...) {
  format_data_common <- function(x, series, type, na.action, demean) {
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

  title_base <- switch(type,
    "correlation" = "Auto-correlation of",
    "covariance" = "Auto-covariance of",
    "partial" = "Auto-partial-correlation of"
  )

  lag.max <- lag.max %||% ceiling(10 * log(nrow(x) / ncol(x), base = 10))
  lag.min <- if (type == "partial") 1 else 0

  UseMethod("setup_acf")
}

#' @noRd
setup_acf.varest <- function(x, series, type, na.action, demean, ...) {
  series <- series %||% names(x$varresult)
  title <- paste(title_base, "Series")

  data <- as.data.frame(stats::residuals(x)) %>%
    format_data_common(series, type, na.action, demean)

  list(data = data, title = title)
}

#' @noRd
#setup_acf.acf <- function(x, series, ...) {}

#' @noRd
setup_acf.default <- function(
    x, series, type, lag.max, na.action, demean, ...) {
  series <- series %||% colnames(x)
  title <- paste(title_base, "VAR Residuals")

  data <- as.data.frame(x) %>%
    format_data_common(series, type, na.action, demean)

  list(data = data, title = title)
}


#' @noRd
test_ccf <- function(
    x, series, type, lag.max, graph_type, ci, facet_type,
    env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$category(type, c("correlation", "covariance"), env)
  test$type(lag.max, c("NULL", "integer", "double"), env)
  test$category(graph_type, c("segment", "area"), env)
  test$interval(ci, 0, 1, FALSE, env)
  test$category(facet_type, c("ggplot", "ggh4x"), env)
}

#'@noRd
setup_ccf <- function(x, series, type, lag.max, na.action, ...) {
  format_data_common <- function(x, series, type, na.action) {
    x %>%
      dplyr::select(dplyr::all_of(series)) %>%
      stats::acf(lag.max, type, plot = FALSE, na.action) %>%
      purrr::pluck("acf") %>%
      purrr::array_tree(3) %>%
      purrr::map2_dfr(series, ~ data.frame(.y, 0:lag.max, .x)) %>%
      purrr::set_names(c("facet_x", "lag", series)) %>%
      tidyr::pivot_longer(-c(facet_x, lag),
        names_to = "facet_y",
        values_to = "value"
      )
  }

  lag.max <- lag.max %||% ceiling(10 * log(nrow(x) / ncol(x), base = 10))

  UseMethod("setup_ccf")
}

#' @noRd
setup_ccf.varest <- function(x, series, type, lag.max, na.action, ...) {
  series <- series %||% names(x$varresult)

  data <- as.data.frame(stats::residuals(x)) %>%
    format_data_common(series, type, na.action)

  list(data = data)
}

#' @noRd
setup_ccf.default <- function(x, series, type, lag.max, na.action, ...) {
  x <- as.data.frame(x)
  x <- setup$ignore_cols(x)

  series <- series %||% colnames(x)

  data <- #x %>%
    format_data_common(x, series, type, na.action)

  list(data = data)
}
