# Helper functions used between more than one method:
stability_helpers <- list()

stability_helpers$format <- function(x, series) {
  purrr::imap_dfr(x[series], function(stab_element, name) {
    data.frame(
      equation = name,
      index = stats::time(stab_element$process),
      value = as.numeric(stab_element$process)
    )
  })
}

stability_helpers$dist <- function(x, ci, ...) {
  strucchange::boundary(x[[1]],
      alpha = 1 - ci, alt.boundary = FALSE, functional = "max", ...
  )
}


# Initial tests and setup (methods at the end):
#' @noRd
stability_test <- function(series, ci, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$interval(ci, 0, 1, FALSE, env)
}

stability_setup <- function(x, series, ci, ...) {
  UseMethod("stability_setup")
}


#' Plot VAR structural stability
#'
#' Plots the result of a [stability][vars::stability] call. Confidence intervals
#' are calculated using [boundary][strucchange::boundary].
#'
#' @param x A "varest" object to pass to [stability][vars::stability], or,
#'  directly, a "varstabil" object.
#' @eval roxy$series()
#' @param ci The level of confidence for the [boundary][strucchange::boundary].
#' @eval roxy$args_gg(c("geom_line", "geom_hline", "facet_wrap"))
#' @eval roxy$dots("stability", "vars::stability")
#'
#' @details
#' `r roxy$details_custom()`
#' `r roxy$details_methods()$stability`
#' 
#' @eval roxy$return_gg()
#' 
#' @eval roxy$fam_diag()
#'
#' @examples
#' ggvar_stability(vars::VAR(freeny[-2]))
#'
#' @export
ggvar_stability <- function(
    x, series = NULL,
    ci = 0.95,
    args_line = list(),
    args_hline = list(linetype = 2, color = "blue"),
    args_facet = list(),
    ...) {
  stability_test(series, ci)

  setup <- stability_setup(x, series, ci, ...)

  graph_add <- inject(list(
    if (!is_false(ci)) {
      ggplot2::geom_hline(yintercept = c(-setup$dist, setup$dist),
        !!!args_hline
      )
    }
  ))

  inject(
    ggplot(setup$data, aes(.data$index, .data$value)) +
    ggplot2::geom_line(!!!args_line) +
    graph_add +
    ggplot2::facet_wrap(vars(.data$equation), !!!args_facet) +
    ggplot2::labs(
      title = "VAR Structural Stability Analisys", x = "Index", y = "Values"
    )
  )
}


#' @noRd
stability_setup.varest <- function(x, series, ci, ...) {
  x <- vars::stability(x, ...)$stability

  series <- series %||% names(x)

  data <- stability_helpers$format(x, series)
  dist <- if (!is_false(ci)) stability_helpers$dist(x, ci, ...)

  list(data = data, dist = dist)
}

#' @noRd
stability_setup.varstabil <- function(x, series, ci, ...) {
  x <- x$stability

  series <- series %||% names(x)

  data <- stability_helpers$format(x, series)
  dist <- if (!is_false(ci)) stability_helpers$dist(x, ci, ...)

  list(data = data, dist = dist)
}
