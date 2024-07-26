# Helper functions used between more than one method:
fit_helpers <- list()

fit_helpers$format <- function(x, series, index) {
  orig_and_fit <- dplyr::bind_cols(
    tibble::tibble(index = index),
    tibble::as_tibble( x$datamat[1:x$K]) %>%
      dplyr::rename_with(~glue("original__{.x}")),
    tibble::as_tibble(stats::fitted(x)) %>%
      dplyr::rename_with(~glue("fitted__{.x}"))
  )

  orig_and_fit %>%
    dplyr::select("index", dplyr::ends_with(series)) %>%
    tidyr::pivot_longer(-"index",
      names_sep = "__", names_to = c("type", "serie"), values_to = "value"
    )
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
fit_test <- function(series, index, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$type(index, c("NULL", "integer", "double"), env)
}

#' @noRd
fit_setup <- function(x, series, index, ..., env = caller_env()) {
  UseMethod("fit_setup")
}


#' Plot VAR fitted values
#'
#' Plots fitted values of a VAR model, versus the actual values.
#' `ggvar_fit` Plots each serie in a facet. `ggvar_fit_colored`
#' plots all in the same graph, each with a different color.
#'
#' @param x A "varest" object to get fitted values from.
#' @eval roxy$series()
#' @eval roxy$index("x$obs")
#' @eval roxy$args_gg(c("geom_line", "facet_wrap"))
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom()`
#' `r roxy$details_methods()$fit`
#' 
#' @eval roxy$return_gg()
#' 
#' @eval roxy$fam_hist()
#'
#' @examples
#' x <- vars::VAR(freeny[-2])
#' ggvar_fit(x, args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_fit <- function(
    x, series = NULL, index = NULL,
    args_line = list(),
    args_facet = list(),
    ...) {
  fit_test(series, index)

  setup <- fit_setup(x, series, index, ...)

  inject(
    ggplot(setup$data, aes(.data$index, .data$value)) +
      ggplot2::geom_line(aes(linetype = .data$type), !!!args_line) +
      ggplot2::facet_wrap(vars(.data$serie), !!!args_facet) +
      ggplot2::labs(
        title = "Fitted VAR Values", x = "Index", y = "Fitted"
      )
  )

}


# Setup methods:
#' @noRd
fit_setup.varest <- function(x, series, index, ..., env) {
  series <- get_series(series, names(x$varresult), env)
  index <- index %||% (x$p + 1):x$totobs

  data <- fit_helpers$format(x, series, index)

  list(data = data)
}
