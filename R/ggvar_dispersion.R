# Helper functions used between more than one method:
dispersion_helpers <- list()

dispersion_helpers$format <- function(x, series) {
  res_and_fit <- dplyr::bind_cols(
    tibble::as_tibble(stats::residuals(x)) %>%
      dplyr::rename_with(~ glue("residual__{.x}")),
    tibble::as_tibble(stats::fitted(x)) %>%
      dplyr::rename_with(~ glue("fitted__{.x}"))
  )

  res_and_fit %>%
    dplyr::select(dplyr::ends_with(series)) %>%
    tidyr::pivot_longer(dplyr::everything(),
      names_sep = "__",
      names_to = c(".value", "serie")
    )
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
dispersion_test <- function(env) {
  with(env, {
    test$type(series, c("NULL", "character"), env = env)
    test$args(
      args_point, args_hline, args_labs, args_facet, env = env
    )
  })
}

#' @noRd
dispersion_setup <- function(x, series, ...) {
  UseMethod("dispersion_setup")
}


#' Plot VAR residuals dispersion
#'
#' Plots a scatterplot of the residuals versus fitted values of a VAR model,
#' using ggplot2.
#'
#' @param x A "varest" object to get residuals and fitted values from.
#' @eval roxy$series()
#' @eval roxy$args_geom(c("geom_point", "geom_hline"))
#' @eval roxy$args_labs()
#' @eval roxy$args_facet()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom()`
#' `r roxy$details_methods()$dispersion`
#'
#' @eval roxy$return_gg()
#'
#' @eval roxy$fam_diag()
#'
#' @examples
#' ggvar_dispersion(vars::VAR(freeny[-2]), args_facet = list(scales = "free_x"))
#'
#' @export
ggvar_dispersion <- function(
    x, series = NULL,
    args_point = list(),
    args_hline = list(yintercept = 0),
    args_labs = list(),
    args_facet = list(),
    ...) {
  # Test and setup:
  env <- current_env()
  dispersion_test(env)
  setup <- dispersion_setup(x, series, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = "VAR Residuals Dispersion", x = "Fitted", y = "Residuals"
  ))

  # Graphs:
  inject(
    ggplot(setup$data, aes(.data$fitted, .data$residual)) +
      geom_point(!!!args_point) +
      geom_hline(!!!args_hline) +
      facet_wrap(vars(.data$serie), !!!args_facet) +
      labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
dispersion_setup.varest <- function(x, series, ..., env) {
  check_dots_empty(error = warn_unempty_dots(x))

  series <- series %||% names(x$varresult)

  data <- dispersion_helpers$format(x, series)

  list(data = data)
}
