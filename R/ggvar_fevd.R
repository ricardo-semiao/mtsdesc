# Helper functions used between more than one method:
fevd_helpers <- list()

fevd_helpers$format <- function(x, series) {
  x %>%
    purrr::imap_dfr(function(fevd_element, name) {
      as.data.frame(fevd_element) %>%
        dplyr::mutate(equation = name, lead = seq_len(nrow(.)))
    }) %>%
    tidyr::pivot_longer(-c("equation", "lead"),
      names_to = "serie", values_to = "value"
    ) %>%
    dplyr::filter(.data$equation %in% series)
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
fevd_test <- function(series, n.ahead, graph_type, env) {
  test$type(series, c("NULL", "character"), env = env)
  test$interval(n.ahead, 1, Inf, env = env)
  test$category(graph_type, c("bar", "area", "line"), env = env)
}

#' @noRd
fevd_setup <- function(x, series, n.ahead, ...) {
  UseMethod("fevd_setup")
}


#' Plot VAR forecast error variance decomposition
#'
#' Plots the result of a [fevd][vars::fevd] call.
#'
#' @param x A "varest" object to pass to [fevd][vars::fevd], or, directly, a
#'  "varfevd" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to
#'  [fevd][vars::fevd]. `r roxy$unused("varfevd")`
#' @eval roxy$series()
#' @eval roxy$graph_type(c("bar", "line"))
#' @eval roxy$args_aes()
#' @eval roxy$args_type()
#' @eval roxy$args_labs()
#' @eval roxy$args_facet()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom(TRUE)`
#' `r roxy$details_methods()$fevd`
#'
#' @eval roxy$return_gg()
#'
#' @eval roxy$fam_output()
#'
#' @examples
#' ggvar_fevd(vars::VAR(freeny[-2]), n.ahead = 10)
#'
#' @export
ggvar_fevd <- function(
    x, series = NULL,
    n.ahead = 10,
    graph_type = "bar",
    args_aes = list(),
    args_type = list(),
    args_labs = list(),
    args_facet = list(),
    ...) {
  # Test and setup:
  env <- current_env()

  fevd_test(series, n.ahead, graph_type, env = env)
  setup <- fevd_setup(x, series, n.ahead, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = "VAR FEVD", x = "Forecast horizon", y = "Variance contribution"
  ))

  if (graph_type == "bar" && length(args_type) == 0) {
    args_type$stat <- "identity"
  }

  args_aes <- update_values(args_aes, graph_type, "Series", env = env) %>%
    process_values(length(setup$series), env = env)

  # Create additions:
  add_type <- inject(switch(graph_type,
    "bar" = list(geom_bar(!!!args_type)),
    "line" = list(geom_line(!!!args_type), geom_point(!!!args_type))
  ))

  add_aes <- define_aes(args_aes, .data$serie)

  # Graph:
  inject(
    ggplot(setup$data, aes(.data$lead, .data$value, !!!add_aes)) +
      add_type +
      facet_wrap(vars(.data$equation), !!!args_facet) +
      define_scales(args_aes) +
      labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
fevd_setup.varest <- function(x, series, n.ahead, ...) {
  x <- vars::fevd(x, n.ahead, ...)

  series <- series %||% names(x)

  data <- fevd_helpers$format(x, series)

  list(data = data, series = series)
}

#' @noRd
fevd_setup.varfevd <- function(x, series, n.ahead, ...) {
  series <- series %||% names(x)

  data <- fevd_helpers$format(x, series)

  list(data = data, series = series)
}
