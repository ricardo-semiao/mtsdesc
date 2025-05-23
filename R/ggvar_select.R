# Helper functions used between more than one method:
select_helpers <- list()

select_helpers$format <- function(x, criteria, trans) {
  criteria <- paste0(criteria, "(n)")

  x$criteria %>%
    t() %>%
    apply(., 2, \(x) if (trans == "index") x / x[1] else x) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::any_of(criteria)) %>%
    dplyr::mutate(lag = 1:nrow(.)) %>%
    tidyr::pivot_longer(-"lag", names_to = "criteria")
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
select_test <- function(env) {
  with(env, {
    test$type(series, c("NULL", "character"), env = env)
    test$interval(lag.max, 1, Inf, env = env)
    test$category(type, c("const", "trend", "both", "none"), env = env)
    test$category(criteria, c("AIC", "HQ", "SC", "FPE"), env = env)
    test$category(trans, c("none", "index"), env = env)
    test$args(
      args_aes, args_line, args_labs, env = env
    )
    test$unused(lag.max, x, "list", env = env)
    test$unused(type, x, "list", env = env)
  })
}

select_setup <- function(x, series, lag.max, type, criteria, trans, ...) {
  UseMethod("select_setup")
}


#' Plot VAR information criteria for lag selection
#'
#' Plots the result of a call to [VARselect][vars::VARselect].
#'
#' @param x A dataset (object coercible to data.frame) to pass to
#'  [VARselect][vars::VARselect], or, directly, a result of such call.
#' @eval roxy$series()
#' @param lag.max Integer for the highest lag order. Passed to
#'  [VARselect][vars::VARselect]. `r roxy$unused("list")`
#' @param type Type of deterministic regressors to include. Passed to
#'  [VARselect][vars::VARselect]. `r roxy$unused("list")`
#' @param criteria The criteria to be considered. Any of "AIC", "HQ", "SC",
#'  and "FPE".
#' @param trans A transformation to apply to each criteria result (vector). Can
#'  be a function, "none" (the default), or "index" to create index numbers.
#' @eval roxy$args_aes()
#' @eval roxy$args_geom("geom_line")
#' @eval roxy$args_labs()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom(TRUE)`
#' `r roxy$details_methods()$select`
#'
#' @eval roxy$return_gg()
#'
#' @eval roxy$fam_diag()
#'
#' @examples
#' ggvar_select(vars::VARselect(freeny[-2]))
#'
#' @export
ggvar_select <- function(
    x, series = NULL,
    lag.max = 10, type = "const",
    criteria = c("AIC", "HQ", "SC", "FPE"), trans = "none",
    args_aes = list(),
    args_line = list(),
    args_labs = list(),
    ...) {
  # Test and setup:
  env <- current_env()
  select_test(env)
  setup <- select_setup(x, series, lag.max, type, criteria, trans, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = "Information Criteria for Each Lag", x = "Lag", y = "value"
  ))

  args_aes <- update_values(args_aes, "line", "Criteria", env = env) %>%
    process_values(length(setup$series), env = env)

  # Create additions:
  add_aes <- define_aes(args_aes, .data$criteria)

  # Graph:
  inject(
    ggplot(setup$data, aes(.data$lag, .data$value, !!!add_aes)) +
      geom_line(!!!args_line) +
      define_scales(args_aes) +
      labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
select_setup.list <- function(x, series, lag.max, type, criteria, trans, ...) {
  series <- series %||% rownames(x$criteria)

  data <- select_helpers$format(x, criteria, trans)

  list(data = data, series = series)
}

#' @noRd
select_setup.default <- function(
    x, series, lag.max, type, criteria, trans, ...) {
  x <- as.data.frame(x)

  series <- series %||% colnames(x)

  x <- vars::VARselect(x[, series], lag.max, type, ...)

  data <- select_helpers$format(x, criteria, trans)

  list(data = data, series = series)
}
