# Helper functions used between more than one method:
history_helpers <- list()

history_helpers$format <- function(x, series, index) {
  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    dplyr::mutate(index = index) %>%
    tidyr::pivot_longer(-"index", values_to = "value", names_to = "serie")
}


# Initial tests and setup (methods at the end):
#' @noRd
history_test <- function(series, index, graph_type, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$type(index, c("NULL", "integer", "double"), env)
  test$category(graph_type, c("faceted", "colored"), env)
}

#' @noRd
history_setup <- function(x, series, index, ...) {
  UseMethod("history_setup")
}


#' Plot Values of Dataset or VAR Residuals
#'
#' Plots the historic values of variables in a dataset, or residuals of a VAR
#'  model. `ggvar_values` Plots each series in a facet.
#'  `ggvar_values_colored` plots all in the same graph, each with a
#'  different color.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval roxy$series()
#' @eval roxy$index(c("x$obs", "nrow(x)"))
#' @eval roxy$graph_type(c("faceted", "colored"), FALSE)
#' @eval roxy$args_gg(c("geom_line", "facet_wrap"))
#' @eval roxy$colors()
#' @eval roxy$dots("history")
#'
#' @eval roxy$return_gg()
#'
#' @examples
#' ggvar_history(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_history(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_history <- function(
    x, series = NULL, index = NULL,
    graph_type = "faceted",
    args_line = list(),
    args_facet = list(),
    colors = NULL,
    ...) {
  history_test(series, index, graph_type)

  setup <- history_setup(x, series, index, ...)

  if (graph_type == "colored") {
    colors <- get_colors(colors, length(setup$series))
  }

  graph_add <- inject(list(
    if (graph_type == "faceted") {
      list(
        ggplot2::geom_line(!!!args_line),
        ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)
      )
    } else if (graph_type == "colored") {
      list(
        ggplot2::geom_line(aes(color = .data$serie), !!!args_line),
        ggplot2::scale_color_manual(values = colors)
      )
    }
  )) %>%
  purrr::list_flatten()
  
  inject(
    ggplot(setup$data, aes(.data$index, .data$value)) +
      graph_add +
      ggplot2::labs(title = setup$title, x = "Index", y = "Values")
  )
}


#' @noRd
history_setup.varest <- function(x, series, index, ...) {
  x <- as.data.frame(stats::residuals(x))

  series <- series %||% colnames(x)
  index <- index %||% seq_len(nrow(x))
  title <- "VAR Residuals Historic Values"

  data <- history_helpers$format(x, series, index)

  list(data = data, series = series, title = title)
}

#' @noRd
history_setup.default <- function(x, series, index, ...) {
  x <- as.data.frame(x) %>% ignore_cols()

  series <- series %||% colnames(x)
  index <- index %||% seq_len(nrow(x))
  title <- "Series Historic Values"

  data <- history_helpers$format(x, series, index)

  list(data = data, series = series, title = title)
}
