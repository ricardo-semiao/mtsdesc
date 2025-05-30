# Helper functions used between more than one method:
history_helpers <- list()

history_helpers$format <- function(x, series, index) {
  x %>%
    dplyr::select(dplyr::all_of(series)) %>%
    dplyr::mutate(index = index) %>%
    tidyr::pivot_longer(-"index", values_to = "value", names_to = "serie")
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
history_test <- function(env) {
  with(env, {
    test$type(series, c("NULL", "character"), env = env)
    test$type(index, c("NULL", "integer", "double"), env = env)
    test$category(faceted, c("TRUE", "FALSE"), env = env)
    test$args(
      args_aes, args_line, args_labs, args_facet, env = env
    )
  })
}

#' @noRd
history_setup <- function(x, series, index, faceted, ..., env) {
  UseMethod("history_setup")
}


#' Plot historic values of dataset or VAR residuals
#'
#' Plots the historic values of variables in a dataset, or residuals of a VAR
#' model. Can separate variables with facets or with a colors (see
#' `graph_type`).
#'
#' @param x Either a "varest" object for plotting the residuals, or an dataset
#'  (object coercible to data.frame) with numeric variables.
#' @eval roxy$series()
#' @eval roxy$index(c("x$obs", "nrow(x)"))
#' @eval roxy$faceted()
#' @eval roxy$args_aes()
#' @eval roxy$args_geom(c("geom_line"))
#' @eval roxy$args_labs()
#' @eval roxy$args_facet()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom(TRUE)`
#' `r roxy$details_methods()$history`
#'
#' @eval roxy$return_gg()
#'
#' @eval roxy$fam_ts()
#' @eval roxy$fam_hist()
#' @eval roxy$fam_diag()
#'
#' @examples
#' ggvar_history(freeny[-2], args_facet = list(scales = "free_y"))
#' ggvar_history(vars::VAR(freeny[-2]), args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_history <- function(
    x, series = NULL, index = NULL,
    faceted = TRUE,
    args_aes = list(),
    args_line = list(),
    args_labs = list(),
    args_facet = list(),
    ...) {
  # Test and setup:
  env <- current_env()
  history_test(env)
  setup <- history_setup(x, series, index, faceted, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = setup$title, x = "Index", y = "Values"
  ))

  if (!faceted) {
    args_aes <- update_values(args_aes, "line", "Series", env = env) %>%
      process_values(length(setup$series), env = env)
  }

  # Create additions:
  add_aes <- if (!faceted) define_aes(args_aes, .data$serie)

  # Graph:
  inject(
    ggplot(setup$data, aes(.data$index, .data$value, !!!add_aes)) +
      geom_line(!!!args_line) +
      {
        if (faceted) facet_wrap(vars(.data$serie), !!!args_facet)
      } +
      define_scales(args_aes) +
      labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
history_setup.varest <- function(x, series, index, faceted, ..., env) {
  check_dots_empty(error = warn_unempty_dots(x))

  x <- as.data.frame(stats::residuals(x))

  series <- series %||% colnames(x)
  index <- index %||% seq_len(nrow(x))
  title <- "VAR Residuals Historic Values"

  data <- history_helpers$format(x, series, index)

  list(data = data, series = series, title = title)
}

#' @noRd
history_setup.default <- function(x, series, index, faceted, ..., env) {
  check_dots_empty(error = warn_unempty_dots(x))

  x <- as.data.frame(x) %>% ignore_cols(env)

  series <- series %||% colnames(x)
  index <- index %||% seq_len(nrow(x))
  title <- "Series Historic Values"

  data <- history_helpers$format(x, series, index)

  list(data = data, series = series, title = title)
}
