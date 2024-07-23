#' Plot Values of Dataset or VAR Residuals
#'
#' Plots the historic values of variables in a dataset, or residuals of a VAR
#'  model. \code{ggvar_values} Plots each series in a facet.
#'  \code{ggvar_values_colored} plots all in the same graph, each with a
#'  different color.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval param_series()
#' @eval param_index("\\code{x$obs} or \\code{nrow(x)}")
#' @eval param_graph_type(c("faceted", "colored"), FALSE)
#' @eval param_args(c("geom_line", "facet_wrap"))
#' @eval param_colors()
#' @eval param_dots("setup_history")
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_history(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_history_colored(freeny[-2])
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
  test_history(series, index, graph_type)

  if (graph_type == "colored") {
    colors <- get_pallete(colors, length(setup$series))
  }

  setup <- setup_history(x, series, index, ...)

  graph_add <- inject(c(
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
  ))
  
  inject(
    ggplot(setup$data, aes(.data$index, .data$value)) +
      graph_add +
      ggplot2::labs(title = setup$title, x = "Index", y = "Values")
  )
}


#' @noRd
test_history <- function(series, index, graph_type, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$type(index, c("NULL", "integer", "double"), env)
  test$category(graph_type, c("faceted", "colored"), env)
}


#' @noRd
setup_history <- function(x, series, index, ...) {
  format_data_common <- function(x, series, index) {
    x %>%
      dplyr::select(dplyr::all_of(series)) %>%
      dplyr::mutate(index = index) %>%
      tidyr::pivot_longer(-c("index"), values_to = "value", names_to = "serie")
  }

  UseMethod("setup_history")
}

#' @noRd
setup_history.varest <- function(x, series, index, ...) {
  series <- series %||% names(x$varresult)
  index <- index %||% (x$p + 1):x$totobs
  title <- "VAR Residuals Historic Values"

  data <- as.data.frame(stats::residuals(x)) %>%
    format_data_common(series, index)

  list(data = data, series = series, index = index, title = title)
}

#' @noRd
setup_history.default <- function(x, series, index, ...) {
  x <- as.data.frame(x)
  x <- setup$ignore_cols(x)

  series <- series %||% colnames(x)
  index <- index %||% seq_len(nrow(x))
  title <- "Series Historic Values"

  data <- x %>%
    format_data_common(series, index)

  list(data = data, series = series, index = index, title = title)
}
