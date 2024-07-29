# Helper functions used between more than one method:
irf_helpers <- list()

irf_helpers$format <- function(x, ci) {
  x %>%
    `[`(if (!is_false(ci)) 1:3 else 1) %>%
    purrr::imap_dfr(function(irf_item, item_name) {
      data.frame( #change to tibble
        serie = item_name,
        purrr::imap_dfr(irf_item, ~ data.frame( #change to tibble
          facet_x = .y, lead = seq_len(nrow(.x)), .x
        ))
      )
    }) %>%
    tidyr::pivot_longer(-c("serie", "facet_x", "lead"),
      names_to = "facet_y", values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = "serie", values_from = "value")
}

irf_helpers$pluck_irf <- function(x, at) {
  at_cur <- at[[1]]
  if (length(at) == 1) {
    as.matrix(x[,at_cur]) %>% `colnames<-`(at_cur)
  } else {
    purrr::map(x[at_cur], ~pluck_irf(.x, at[-1]))
  }
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
irf_test <- function(
    series_imp, series_resp, n.ahead, ci, facet_type, env) {
  test$type(series_imp, c("NULL", "character"), env)
  test$type(series_resp, c("NULL", "character"), env)
  test$interval(n.ahead, 1, Inf, env = env)
  test$interval(ci, 0, 1, FALSE, env = env)
  test$category(facet_type, c("ggplot", "ggh4x"))
}

#' @noRd
irf_setup <- function(
    x, series_imp, series_resp, n.ahead, ci, ..., env) {
  UseMethod("irf_setup")
}


#' Plot VAR impulse response functions
#'
#' Plots the result of a [irf][vars::irf] call.
#'
#' @param x A "varest" object to pass to [irf][vars::irf], or, directly, a
#'  "varirf" object.
#' @param series_imp A character vector with variables to consider for the
#'  impulses. Defaults to all (`NULL`).
#' @param series_resp A character vector with variables to consider for the
#'  responses. Defaults to all (`NULL`).
#' @param n.ahead An integer. The size of the forecast horizon, passed to
#'  [irf][vars::irf]. `r roxy$unused("varirf")`
#' @eval roxy$ci("vars::irf")
#' @eval roxy$args_geom(c("geom_line", "geom_hline", "geom_ribbon"))
#' @eval roxy$args_labs()
#' @eval roxy$args_facet()
#' @eval roxy$facet_type()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom()`
#' `r roxy$details_methods()$irf`
#' 
#' @eval roxy$return_gg()
#' 
#' @eval roxy$fam_output()
#'
#' @examples
#' ggvar_irf(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_irf <- function(
    x, series_imp = NULL, series_resp = NULL,
    n.ahead = 10, ci = 0.95,
    args_line = list(),
    args_hline = list(yintercept = 0),
    args_ribbon = list(fill = NA, linetype = 2, color = "blue"),
    args_labs = list(),
    args_facet = list(),
    facet_type = "ggplot",
    ...) {
  # Test and setup:
  env <- current_env()

  irf_test(series_imp, series_resp, n.ahead, ci, facet_type, env = env)
  setup <- irf_setup(x, series_imp, series_resp, n.ahead, ci, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = "VAR Impulse Response Functions", x = "Forecast horizon",
    y = "Effect"
  ))

  # Create additions:
  add_ribbon <- inject(if (!is_false(ci)) {
    list(
      geom_ribbon(aes(ymin = .data$Lower, ymax = .data$Upper), !!!args_ribbon)
    )
  })

  add_facet <- define_facet_grid(facet_type, args_facet, env = env)

  # Graph:
  inject(
    ggplot(setup$data, aes(.data$lead, .data$irf)) +
    add_ribbon +
    geom_line(!!!args_line) +
    geom_hline(!!!args_hline) +
    add_facet +
    define_sec_axis("Impulse", "Response") +
    labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
irf_setup.varest <- function(
    x, series_imp, series_resp, n.ahead, ci, ..., env) {
  series_imp <- get_series(series_imp, names(x$varresult), env)
  series_resp <- get_series(series_resp, names(x$varresult), env)
  
  x <- vars::irf(x, series_imp, series_resp, n.ahead,
    boot = !is_false(ci), ci = ci, ...
  )

  data <- irf_helpers$format(x, ci)

  list(data = data)
}

#' @noRd
irf_setup.varirf <- function(
    x, series_imp, series_resp, n.ahead, ci, ..., env) {
  series_imp <- get_series(series_imp, x$impulse, env)
  series_resp <- get_series(series_resp, x$response, env)

  x <- irf_helpers$pluck_irf(x, list(1:3, series_imp, series_resp))

  data <- irf_helpers$format(x, ci)

  list(data = data)
}
