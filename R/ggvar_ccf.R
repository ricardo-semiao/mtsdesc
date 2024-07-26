# Helper functions used between more than one method:
ccf_helpers <- list()

ccf_helpers$format <- function(x, series, lag.max, type, ...) {
  lag.max <- lag.max %||% ceiling(10 * log(nrow(x) / ncol(x), base = 10))

  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    stats::acf(lag.max, type, plot = FALSE, ...) %>%
    purrr::pluck("acf") %>%
    purrr::array_tree(3) %>%
    purrr::map2_dfr(series, ~ data.frame(.y, 0:lag.max, .x)) %>%
    purrr::set_names(c("facet_x", "lag", series)) %>%
    tidyr::pivot_longer(-c("facet_x", "lag"),
      names_to = "facet_y",
      values_to = "value"
    )
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
ccf_test <- function(
    x, series, lag.max, type, graph_type, ci, facet_type,
    env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$category(graph_type, c("segment", "area"), env)
  test$interval(ci, 0, 1, FALSE, env)
  test$category(facet_type, c("ggplot", "ggh4x"), env)

  test$category(type, c("correlation", "covariance"), env)
  test$type(lag.max, c("NULL", "integer", "double"), env)
}

#'@noRd
ccf_setup <- function(x, series, lag.max, type, ci, ...) {
  UseMethod("ccf_setup")
}


#' @rdname ggvar_acf
#' 
#' @eval roxy$facet_type()
#' 
#' @export
ggvar_ccf <- function(
    x, series = NULL,
    lag.max = NULL, type = "correlation",
    graph_type = "segment",
    args_type = list(),
    args_ribbon = list(linetype = 2, color = "blue", fill = NA),
    args_hline = list(),
    args_facet = list(),
    facet_type = "ggplot",
    ci = 0.95,
    ...) {
  ccf_test(x, series, lag.max, type, graph_type, ci, facet_type)

  title <- switch(type,
    "correlation" = "Cross-correlation of Series",
    "covariance" = "Cross-covariance of Series"
  )

  setup <- ccf_setup(x, series, lag.max, type, ci, ...)

  graph_add <- inject(list(
    if (graph_type == "segment") {
      ggplot2::geom_segment(aes(xend = .data$lag, yend = 0), !!!args_type)
    } else if (graph_type == "area") {
      ggplot2::geom_area(aes(y = .data$value), !!!args_type)
    },
    if (!is_false(ci)) {
      dist <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(setup$data))
      ggplot2::geom_ribbon(aes(ymin = -dist, ymax = dist), !!!args_ribbon)
    },
    define_facet(facet_type, "facet_x", "facet_y", !!!args_facet)
  ))

  inject(
    ggplot(setup$data, aes(.data$lag, .data$value)) +
      graph_add +
      ggplot2::geom_hline(yintercept = 0, !!!args_hline) +
      ggplot2::labs(title = title, x = "Lags", y = "Values")
  )
}


# Setup methods:
#' @noRd
ccf_setup.varest <- function(x, series, lag.max, type, ci, ...) {
  x <- as.data.frame(stats::residuals(x))

  series <- series %||% colnames(x)

  data <- ccf_helpers$format(x, series, lag.max, type, ...)

  list(data = data)
}

#' @noRd
ccf_setup.default <- function(x, series, lag.max, type, ci, ...) {
  x <- as.data.frame(x) %>% ignore_cols()

  series <- series %||% colnames(x)

  data <- ccf_helpers$format(x, series, lag.max, type, ...)

  list(data = data)
}
