# Helper functions used between more than one method:
distribution_helpers <- env()

distribution_helpers$format_hist <- function(x, series) {
  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    tidyr::pivot_longer(dplyr::everything(),
      names_to = "serie", values_to = "residual"
    )
}

distribution_helpers$format_dens <- function(x, series) {
  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    purrr::imap_dfr(function(col, colname) {
      tibble::tibble(
        residual = seq(min(col), max(col), length = 200),
        serie = colname,
        density = stats::dnorm(.data$residual, sd = stats::sd(col))
      )
    })
}


# Initial tests and setup (methods at the end):
#' @noRd
distribution_test <- function(series, plot_normal, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$type(plot_normal, c("TRUE", "FALSE"), env)
}

#' @noRd
distribution_setup <- function(x, series, plot_normal, ...) {
  UseMethod("distribution_setup")
}


#' Plot VAR Residuals Distribution
#'
#' Plots the histogram of the residuals of a VAR model, or of the variables in a
#'  dataset, possibly overlapped with a normal curve.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval roxy$series()
#' @param plot_normal Logical, whether or not a normal curve should be plotted.
#' @eval roxy$args_gg(c("geom_histogram", "geom_line", "facet_wrap"))
#' @eval roxy$dots("distribution")
#'
#' @eval roxy$return_gg()
#'
#' @examples
#' ggvar_distribution(vars::VAR(freeny[-2]))
#'
#' @export
ggvar_distribution <- function(
    x, series = NULL,
    plot_normal = TRUE,
    args_histogram = list(bins = 30),
    args_line = list(),
    args_facet = list(),
    ...) {
  distribution_test(series, plot_normal)

  setup <- distribution_setup(x, series, plot_normal, ...)

  graph_add <- inject(list(
    if (plot_normal) {
      ggplot_add <- ggplot2::geom_line(aes(y = .data$density),
        !!!args_line, data = setup$data_dens
      )
    }
  ))

  inject(
    ggplot(setup$data_hist, aes(x = .data$residual)) +
      ggplot2::geom_histogram(aes(y = ggplot2::after_stat(.data$density)),
        !!!args_histogram
      ) +
      graph_add +
      ggplot2::facet_wrap(vars(.data$serie), !!!args_facet) +
      ggplot2::labs(title = setup$title, x = "Values", y = "Density")
  )
}


#' @noRd
distribution_setup.varest <- function(x, series, plot_normal, ...) {
  x <- as.data.frame(stats::residuals(x))

  title <- "VAR Residuals Distribution"
  series <- series %||% colnames(x)

  data_hist <- distribution_helpers$format_hist(x, series)
  data_dens <- if (plot_normal) distribution_helpers$format_dens(x, series)

  list(data_hist = data_hist, data_dens = data_dens, title = title)
}

#' @noRd
distribution_setup.default <- function(x, series, plot_normal, ...) {
  x <- as.data.frame(x) %>% ignore_cols()

  title <- "Time Series Distribution"
  series <- series %||% colnames(x)

  data_hist <- distribution_helpers$format_hist(x, series)
  data_dens <- if (plot_normal) distribution_helpers$format_dens(x, series)

  list(data_hist = data_hist, data_dens = data_dens, title = title)
}
