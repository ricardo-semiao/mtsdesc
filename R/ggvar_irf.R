#' @noRd
setup_ggvar_irf <- function(
    x, n.ahead, series_impulse, series_response, ci, facet) {
  test$class_arg(x, c("varirf", "varest"))
  test$series(series_impulse, x, "impulse")
  test$series(series_response, x, "response")
  test$categorical_arg(facet, c("ggplot", "ggh4x"))
  test$interval_arg(ci, 0, 1, FALSE)
  stopifnot(
    "`n.ahead` must be supplied with `x` of class 'varest'" =
      inherits(x, "varirf") || is.numeric(n.ahead)
  )

  list(
    series_impulse = series_impulse %||% get_names(x, "impulse"),
    series_response = series_response %||% get_names(x, "response")
  )
}

#' Plot Impulse Response Functions of a VAR
#'
#' Plots the result of a \link[vars]{irf} call.
#'
#' @param x A "varest" object to pass to \link[vars]{irf}, or, directly, a
#'  "varirf" object.
#' @param n.ahead An integer. The size of the forecast horizon, passed to
#'  \link[vars]{irf}. Unused if \code{x} is "varirf".
#' @param series_impulse A character vector with variables to consider for the
#'  impulses. Defaults to all (\code{NULL}).
#' @param series_response A character vector with variables to consider for the
#'  responses. Defaults to all (\code{NULL}).
#' @param ci The level of confidence for the \link[vars]{irf}. Set to
#'  \code{FALSE} to omit.
#' @param ... Additional arguments passed to \link[vars]{irf}.
#' @eval roxy$facet_type()
#' @eval roxy$args_gg(c("geom_line", "geom_hline", "geom_ribbon", "facet_grid"))
#'
#' @return An object of class \code{ggplot}.
#'
#' @examples
#' ggvar_irf(vars::VAR(freeny[-2]), n.ahead = 10,
#'  args_facet = list(scales = "free_y")
#' )
#'
#' @export
ggvar_irf <- function(
    x, n.ahead = NULL, series_impulse = NULL, series_response = NULL,
    ci = 0.95, ...,
    facet = "ggplot",
    args_line = list(),
    args_hline = list(),
    args_ribbon = list(fill = NA, linetype = 2, color = "blue"),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_irf(
    x, n.ahead, series_impulse, series_response, ci, facet
  )
  reassign <- c("series_impulse", "series_impulse")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  irf <- if (inherits(x, "varest")) {
    vars::irf(x, series_impulse, series_response, n.ahead,
      boot = !isFALSE(ci), ci = ci, ...
    )
  } else {
    x
  }

  data <- irf %>%
    magrittr::extract(if (!isFALSE(ci)) 1:3 else 1) %>%
    purrr::imap_dfr(function(x, name) {
      data.frame(
        serie = name,
        purrr::imap_dfr(x, ~ data.frame(
          effect_on = .y, lead = seq_len(nrow(.x)), .x
        ))
      )
    }) %>%
    tidyr::pivot_longer(-c("serie", "effect_on", "lead"),
      names_to = "effect_of", values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = "serie", values_from = "value")

  # Graph:
  ggplot_add <- list(
    if (!isFALSE(ci)) {
      inject(ggplot2::geom_ribbon(aes(ymin = .data$Lower, ymax = .data$Upper),
        !!!args_ribbon
      ))
    },
    inject(define_facet(facet, "effect_of", "effect_on", !!!args_facet))
  )

  ggplot(data, aes(.data$lead, .data$irf)) +
    ggplot_add +
    inject(ggplot2::geom_line(!!!args_line)) +
    inject(ggplot2::geom_hline(yintercept = 0, !!!args_hline)) +
    create_sec_axis() +
    ggplot2::labs(
      title = "VAR Impulse Response Functions",
      x = "Forecast horizon", y = "Effect"
    )
}

#' @noRd
ggvar_irf_ind <- function(
    x, series_impulse, series_response, n.ahead = NULL,
    ci = 0.95, ...,
    facet = "ggplot",
    args_line = list(),
    args_hline = list(),
    args_ribbon = list(fill = NA, linetype = 2, color = "blue"),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_irf(
    x, n.ahead, series_impulse, series_response, ci, facet
  )
  reassign <- c("series_impulse", "series_impulse")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  irf <- if (inherits(x, "varest")) {
    vars::irf(x, series_impulse, series_response, n.ahead,
              boot = !isFALSE(ci), ci = ci, ...
    )
  } else {
    x
  }

  data <- purrr::map_dfc(irf[1:3], ~ .x[[series_response]][,series_impulse]) %>%
    dplyr::mutate(horizon = 1:nrow(.))

  # Graph:
  ggplot_add <- list(
    if (!isFALSE(ci)) {
      inject(ggplot2::geom_ribbon(aes(ymin = .data$Lower, ymax = .data$Upper),
                                  !!!args_ribbon
      ))
    }
  )

  ggplot(data, aes(.data$lead, .data$irf)) +
    ggplot_add +
    inject(ggplot2::geom_line(!!!args_line)) +
    inject(ggplot2::geom_hline(yintercept = 0, !!!args_hline)) +
    ggplot2::labs(
      title = "VAR Impulse Response Functions",
      x = "Forecast horizon", y = "Effect"
    )
}
