#' Plot VAR Residuals Distribution
#'
#' Plots the histogram of the residuals of a VAR model, or of the variables in a
#'  dataset, possibly overlapped with a normal curve.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval param_series()
#' @param plot_normal Logical, whether or not a normal curve should be plotted.
#' @eval param_args(c("geom_histogram", "geom_line", "facet_wrap"))
#' @eval param_dots("setup_distribution")
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_distribution(vars::VAR(freeny[-2]))
#'
#' @export
ggvar_distribution <- function(
    x, series = NULL, plot_normal = TRUE,
    args_histogram = list(bins = 30),
    args_line = list(),
    args_facet = list(),
    ...) {
  test_distribution(series, plot_normal)

  setup <- setup_distribution(x, series, plot_normal, ...)

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
test_distribution <- function(series, plot_normal, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$type(plot_normal, c("TRUE", "FALSE"), env)
}

#' @noRd
setup_distribution <- function(x, series, plot_normal, ...) {
  UseMethod("setup_distribution")
}

#' @noRd
setup_distribution.varest <- function(x, series, plot_normal, ...) {
  x <- as.data.frame(stats::residuals(x))

  title <- "VAR Residuals Distribution"
  series <- series %||% colnames(x)

  data_hist <- format_distribution_common()$format_hist(x, series)
  data_density <- if (plot_normal) {
    format_distribution_common()$format_density(x, series)
  }

  list(data_hist = data_hist, data_density = data_density, title = title)
}

#' @noRd
setup_distribution.default <- function(x, series, plot_normal, ...) {
  x <- as.data.frame(x)

  title <- "Time Series Distribution"
  series <- series %||% colnames(x)

  data_hist <- format_distribution_common()$format_hist(x, series)
  data_dens <- if (plot_normal) {
    format_distribution_common()$format_dens(x, series)
  }

  list(data_hist = data_hist, data_dens = data_dens, title = title)
}

#' @noRd
format_distribution_common <- function() {
  assets <- list()

  assets$format_hist <- function(x, series) {
    x %>%
      dplyr::select(dplyr::all_of(series)) %>%
      tidyr::pivot_longer(dplyr::everything(),
        names_to = "serie", values_to = "residual"
      )
  }

  assets$format_dens <- function(x, series) {
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

  assets
}