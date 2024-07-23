# Helper functions used between more than one method:
fevd_helpers <- env()

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


#' @noRd
test_fevd <- function(series, n.ahead, graph_type) {
  test$type(series, c("NULL", "character"), env)
  test$interval(n.ahead, 1, Inf, env = env)
  test$category(graph_type, c("bar", "area", "line"), env)
}

#' @noRd
setup_fevd <- function(x, series, n.ahead, ...) {
  UseMethod("setup_fevd")
}


#' Plot for Forecast Error Variance Decomposition of a VAR
#'
#' Plots the result of a \link[vars]{fevd} call.
#'
#' @param x A "varest" object to pass to \link[vars]{fevd}, or, directly, a
#'  "varfevd" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to
#'  \link[vars]{fevd}. Unused if `x` is a "varfevd" object.
#' @eval param_series()
#' @eval param_graph_type(c("geom_segment", "geom_area", "geom_line"))
#' @eval param_args_geom()
#' @eval param_args(c("facet_wrap", "geom_point"))
#' @eval param_colors()
#' @param ... If \code{geom="line"}, additional arguments to
#'  \link[ggplot2]{geom_point}
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_fevd(vars::VAR(freeny[-2]), n.ahead = 10)
#'
#' @export
ggvar_fevd <- function(
    x, series = NULL,
    n.ahead = NULL,
    graph_type = "bar",
    args_geom = list(),
    args_facet = list(),
    args_point = list(),
    colors = NULL,
    ...) {
  test_fevd(series, n.ahead, graph_type)

  setup <- setup_fevd(x, series, n.ahead, ...)

  colors <- get_pallete(colors, length(setup$series))

  graph_add <- inject(list(
    if (graph_type == "bar") {
      ggplot2::geom_bar(aes(fill = .data$serie),
        stat = "identity", !!!args_geom
      )
    } else if ("area") {
      ggplot2::geom_area(aes(fill = .data$serie), !!!args_geom)
    } else if ("line") {
      list(
        ggplot2::geom_line(aes(color = .data$serie), !!!args_geom),
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
setup_fevd.varest <- function(x, series, n.ahead, ...) {
  x <- vars::fevd(x, n.ahead)
  
  series <- series %||% names(x)

  data <- fevd_helpers$format(x, series)

  list(data = data, series = series)
}

#' @noRd 
setup_fevd.fevd <- function(x, series, n.ahead, ...) {
  series <- series %||% names(x)

  data <- fevd_helpers$format(x, series)
  
  list(data = data, series = series)
}
