# Helper functions used between more than one method:
distribution_helpers <- list()

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


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
distribution_test <- function(env) {
  with(env, {
    test$type(series, c("NULL", "character"), env = env)
    test$type(plot_normal, c("TRUE", "FALSE"), env = env)
    test$args(
      args_histogram, args_line, args_labs, args_facet, env = env
    )
  })
}

#' @noRd
distribution_setup <- function(x, series, ...) {
  UseMethod("distribution_setup")
}


#' Plot VAR residuals distribution
#'
#' Plots the histogram of the residuals of a VAR model, or of the variables in a
#' dataset, possibly overlapped with a normal curve.
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval roxy$series()
#' @param plot_normal Logical, whether or not a normal curve should be plotted.
#' @eval roxy$args_geom(c("geom_histogram", "geom_line"))
#' @eval roxy$args_labs()
#' @eval roxy$args_facet()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom()`
#' `r roxy$details_methods()$distribution`
#'
#' @eval roxy$return_gg()
#'
#' @eval roxy$fam_diag()
#' @eval roxy$fam_ts()
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
    args_labs = list(),
    args_facet = list(),
    ...) {
  # Test and setup:
  env <- current_env()
  distribution_test(env)
  setup <- distribution_setup(x, series, plot_normal, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = setup$title, x = "Values", y = "Density"
  ))

  # Create additions:
  add_extra <- inject(if (plot_normal) {
    list(geom_line(aes(y = .data$density), setup$data_dens, !!!args_line))
  })

  # Graph:
  inject(
    ggplot(setup$data_hist, aes(x = .data$residual)) +
      geom_histogram(aes(y = after_stat(.data$density)), !!!args_histogram) +
      add_extra +
      facet_wrap(vars(.data$serie), !!!args_facet) +
      labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
distribution_setup.varest <- function(x, series, plot_normal, ..., env) {
  check_dots_empty(error = warn_unempty_dots(x))

  x <- as.data.frame(stats::residuals(x))

  title <- "VAR Residuals Distribution"
  series <- series %||% colnames(x)

  data_hist <- distribution_helpers$format_hist(x, series)
  data_dens <- if (plot_normal) distribution_helpers$format_dens(x, series)

  list(data_hist = data_hist, data_dens = data_dens, title = title)
}

#' @noRd
distribution_setup.default <- function(x, series, plot_normal, ..., env) {
  check_dots_empty(error = warn_unempty_dots(x))

  x <- as.data.frame(x) %>% ignore_cols(env)

  title <- "Time Series Distribution"
  series <- series %||% colnames(x)

  data_hist <- distribution_helpers$format_hist(x, series)
  data_dens <- if (plot_normal) distribution_helpers$format_dens(x, series)

  list(data_hist = data_hist, data_dens = data_dens, title = title)
}
