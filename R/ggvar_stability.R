# Helper functions used between more than one method:
stability_helpers <- env()

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
test_stability <- function(series, ci, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$interval(ci, 0, 1, FALSE, env)
}

setup_stability <- function(x, series, ci, ...) {
  UseMethod("setup_stability")
}


#' Plot for Structural Stability of a VAR
#'
#' Plots the result of a \link[vars]{stability} call. Confidence intevals are
#'  calculated using \link[strucchange]{boundary}.
#'
#' @param x A "varest" object to pass to \link[vars]{stability}, or, directly, a
#'  "varstabil" object.
#' @eval param_series()
#' @param ci The level of confidence for the \link[strucchange]{boundary}.
#' @eval param_args(c("geom_line", "args_hline", "facet_wrap"))
#' @eval param_dots("stability", "vars::stability")
#'
#' @return An object of class \code{ggplot}.
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
  test_stability(series, ci)

  setup <- setup_stability(x, series, ci, ...)

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
setup_stability.varest <- function(x, series, ci, ...) {
  x <- vars::stability(x, ...)$stability

  series <- series %||% names(x)

  data <- stability_helpers$format(x, series)
  dist <- if (!is_false(ci)) stability_helpers$dist(x, ci, ...)

  list(data = data, dist = dist)
}

#' @noRd
setup_stability.varstabil <- function(x, series, ci, ...) {
  x <- x$stability

  series <- series %||% names(x)

  data <- stability_helpers$format(x, series)
  dist <- if (!is_false(ci)) stability_helpers$dist(x, ci, ...)

  list(data = data, dist = dist)
}
