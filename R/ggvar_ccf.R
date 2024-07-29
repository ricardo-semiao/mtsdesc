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
    series, lag.max, type, graph_type, ci, facet_type, env = caller_env()) {
  test$type(series, c("NULL", "character"), env = env)
  test$category(type, c("correlation", "covariance"), env)
  test$interval(lag.max, 1, Inf, NULL, env = env)
  test$category(graph_type, c("segment", "area"), env = env)
  test$category(facet_type, c("ggplot", "ggh4x"), env = env)
  test$interval(ci, 0, 1, FALSE, env = env)
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
    args_hline = list(yintercept = 0),
    args_labs = list(),
    args_facet = list(),
    facet_type = "ggplot",
    ci = 0.95,
    ...) {
  # Test and setup:
  ccf_test(series, lag.max, type, graph_type, ci, facet_type)

  setup <- ccf_setup(x, series, lag.max, type, ci, ...)

  setup$title <- switch(type,
    "correlation" = "Cross-correlation of Series",
    "covariance" = "Cross-covariance of Series"
  )


  # Update arguments and create additions:
  args_labs <- update_labs(args_labs, list(
    title = setup$title, x = "Lags", y = "Values"
  ))

  add_type <- inject(switch(graph_type,
    "segment" = list(
      geom_segment(aes(xend = .data$lag, yend = 0), !!!args_type)
    ),
    "area" = list(
      geom_area(aes(y = .data$value), !!!args_type)
    )
  ))

  add_ribbon <- inject(list(
    if (!is_false(ci)) {
      dist <- stats::qnorm((1 - ci) / 2) / sqrt(nrow(setup$data))
      geom_ribbon(aes(ymin = -dist, ymax = dist), !!!args_ribbon)
    }
  ))

  add_facet <- inject(define_facet_grid(facet_type, !!!args_facet))


  # Graph:
  inject(
    ggplot(setup$data, aes(.data$lag, .data$value)) +
      add_type +
      add_ribbon +
      geom_hline(!!!args_hline) +
      add_facet +
      labs(!!!args_labs)
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
  x <- as.data.frame(x) %>% ignore_cols(env)

  series <- series %||% colnames(x)

  data <- ccf_helpers$format(x, series, lag.max, type, ...)

  list(data = data)
}
