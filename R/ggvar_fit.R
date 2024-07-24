#' @noRd
setup_ggvar_fit <- function(x, compare, series, index) {
  test$class_arg(x, c("varest"))
  test$series(series, x)
  test$index(index, n = x$totobs)
  test$boolean_arg(compare)

  list(
    series = series %||% get_names(x),
    guide = if (compare) "legend" else "none", name = "Type"
  )
}

#' @noRd
data_ggvar_fit <- function(x, compare, series, index) {
  data.frame(index = index[(x$p + 1):x$totobs], Fitted.. = stats::fitted(x)) %>%
    `if`(compare, cbind(., Original.. = x$datamat[1:x$K]), .) %>%
    dplyr::select(c(index, dplyr::ends_with(series))) %>%
    tidyr::pivot_longer(-index,
      names_to = c("type", "serie"), names_sep = "\\.\\.\\.", values_to = "value"
    )
}

#' Plot VAR Fitted Values
#'
#' Plots fitted values of a VAR model, versus the actual values.
#'  \code{ggvar_fit} Plots each serie in a facet. \code{ggvar_fit_colored}
#'  plots all in the same graph, each with a different color.
#'
#' @param x A "varest" object to get fitted values from.
#' @param compare Logical, should the "original" values be plotted?
#' @eval param_series()
#' @eval param_index("\\code{x$obs}")
#' @eval param_colors()
#' @eval param_linetypes()
#' @eval param_args(c("geom_line", "facet_wrap"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' x <- vars::VAR(freeny[-2])
#' ggvar_fit(x, args_facet = list(scales = "free_y"))
#' ggvar_fit_colored(x)
#'
#' @export
ggvar_fit <- function(
    x, compare = TRUE, series = NULL, index = 1:x$totobs,
    linetypes = c("solid", "dashed"),
    args_line = list(),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_fit(x, compare, series, index)
  reassign <- c("series")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  data <- data_ggvar_fit(x, compare, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    inject(ggplot2::geom_line(aes(linetype = .data$type), !!!args_line)) +
    inject(ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)) +
    ggplot2::scale_linetype_manual(values = linetypes, guide = setup$guide) +
    ggplot2::labs(
      title = "Fitted VAR Values", x = "Index", y = "Fitted",
      color = "Serie", linetype = "Type"
    )
}

#' @rdname ggvar_fit
#' @export
ggvar_fit_colored <- function(
    x, compare = TRUE, series = NULL, index = 1:x$totobs,
    colors = NULL, linetypes = c("solid", "dashed"),
    args_line = list()) {
  # Setup:
  setup <- setup_ggvar_fit(x, compare, series, index)
  reassign <- c("series", "colors")
  list2env(setup[reassign], envir = rlang::current_env())

  colors <- get_colors(colors, length(series))

  # Data:
  data <- data_ggvar_fit(x, compare, series, index)

  # Graph:
  ggplot(data, aes(.data$index, .data$value)) +
    inject(ggplot2::geom_line(aes(color = .data$serie, linetype = .data$type),
      !!!args_line
    )) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_linetype_manual(values = linetypes, guide = setup$guide) +
    ggplot2::labs(
      title = "Fitted VAR Values", x = "Index", y = "Fitted", color = "Serie"
    )
}
