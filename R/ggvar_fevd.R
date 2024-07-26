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


# Initial tests and setup (methods at the end):
#' @noRd
fevd_test <- function(series, n.ahead, graph_type, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$interval(n.ahead, 1, Inf, env = env)
  test$category(graph_type, c("bar", "area", "line"), env)
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
#'  [fevd][vars::fevd]. Unused if `x` is a "varfevd" object.
#' @eval roxy$series()
#' @eval roxy$graph_type(c("segment", "area", "line"))
#' @eval roxy$args_type()
#' @eval roxy$args_gg(c("facet_wrap", "geom_point"))
#' @eval roxy$colors()
#' @eval roxy$dots("fevd", "vars::fevd")
#'
#' @details
#' `r roxy$details_custom()`
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
    n.ahead = NULL,
    graph_type = "bar",
    args_type = list(),
    args_facet = list(),
    args_point = list(),
    colors = NULL,
    ...) {
  fevd_test(series, n.ahead, graph_type)

  setup <- fevd_setup(x, series, n.ahead, ...)

  colors <- get_colors(colors, length(setup$series))

  graph_add <- inject(list(
    if (graph_type == "bar") {
      ggplot2::geom_bar(aes(fill = .data$serie),
        stat = "identity", !!!args_type
      )
    } else if ("area") {
      ggplot2::geom_area(aes(fill = .data$serie), !!!args_type)
    } else if ("line") {
      list(
        ggplot2::geom_line(aes(color = .data$serie), !!!args_type),
        ggplot2::geom_point(aes(color = .data$serie), !!!args_point)
      )
    }
  )) %>%
  purrr::list_flatten()

  inject(
    ggplot(setup$data, aes(.data$lead, .data$value)) +
      graph_add +
      ggplot2::facet_wrap(vars(.data$equation), !!!args_facet) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        title = "VAR FEVD", x = "Forecast horizon",
        y = "Variance contribution", fill = "Serie"
      )
  )
}


#' @noRd 
fevd_setup.varest <- function(x, series, n.ahead, ...) {
  x <- vars::fevd(x, n.ahead, ...)
  
  series <- series %||% names(x)

  data <- fevd_helpers$format(x, series)

  list(data = data, series = series)
}

#' @noRd 
fevd_setup.fevd <- function(x, series, n.ahead, ...) {
  series <- series %||% names(x)

  data <- fevd_helpers$format(x, series)
  
  list(data = data, series = series)
}
