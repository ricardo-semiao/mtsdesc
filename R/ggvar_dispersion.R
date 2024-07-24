# Helper functions used between more than one method:
dispersion_helpers <- env()

dispersion_helpers$format <- function(x, series) {
  res_and_fit <- dplyr::bind_cols(
    tibble::as_tibble(stats::residuals(x)) %>%
      dplyr::rename_with(~glue("residual__{.x}")),
    tibble::as_tibble(stats::fitted(x)) %>%
      dplyr::rename_with(~glue("fitted__{.x}"))
  )

  res_and_fit %>%
    dplyr::select(dplyr::ends_with(series)) %>%
    tidyr::pivot_longer(dplyr::everything(),
      names_sep = "__",
      names_to = c(".value", "serie")
    )
}


# Initial tests and setup (methods at the end):
#' @noRd
test_dispersion <- function(series, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
}

#' @noRd 
setup_dispersion <- function(x, series, ...) {
  UseMethod("setup_dispersion")
}


#' Plot VAR Residuals Dispersion
#'
#' Plots a scatterplot of the residuals versus fitted values of a VAR model,
#'  using ggplot2.
#'
#' @param x A "varest" object to get residuals and fitted values from.
#' @eval roxy$series()
#' @eval roxy$args(c("geom_point", "geom_hline", "facet_wrap"))
#' @eval roxy$dots("dispersion")
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_dispersion(vars::VAR(freeny[-2]), args_facet = list(scales = "free_x"))
#'
#' @export
ggvar_dispersion <- function(
    x, series = NULL,
    args_point = list(),
    args_hline = list(),
    args_facet = list(),
    ...) {
  test_dispersion(series)

  setup <- setup_dispersion(x, series, ...)

  inject(
    ggplot(setup$data, aes(.data$fitted, .data$residual)) +
      ggplot2::geom_point(!!!args_point) +
      ggplot2::geom_hline(yintercept = 0, !!!args_hline) +
      ggplot2::facet_wrap(vars(.data$serie), !!!args_facet) +
      ggplot2::labs(
        title = "VAR Residuals Dispersion", x = "Fitted", y = "Residuals"
      )
  )
}


#' @noRd 
setup_dispersion.varest <- function(x, series, ...) {
  series <- series %||% names(x$varresult)
  
  data <- dispersion_helpers$format(x, series)

  list(data = data)
}
