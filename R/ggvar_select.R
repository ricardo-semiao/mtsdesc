# Helper functions used between more than one method:
select_helpers <- list()

select_helpers$format <- function(x, criteria, trans) {
  x$criteria %>%
    t() %>%
    apply(., 2, \(x) if (trans == "index") {x/x[1]} else {x}) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::any_of(criteria)) %>%
    dplyr::mutate(lag = 1:nrow(.)) %>%
    tidyr::pivot_longer(-"lag")
}

# Initial tests and setup (methods at the end):
#' @noRd
select_test <- function(
    series, lag.max, type, criteria, trans, env = caller_env()) {
  test$type(series, c("NULL", "character"), env)
  test$interval(lag.max, 1, Inf, env = env)
  test$category(type, c("const", "trend", "both", "none"), env)
  test$category(criteria, c("AIC", "HQ", "SC", "FPE"), env)
  test$category(trans, c("none", "index"), env)
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
#'  [VARselect][vars::VARselect].
#' @param type Type of deterministic regressors to include. Passed to
#'  [VARselect][vars::VARselect].
#' @param criteria The criteria to be considered. Any of "AIC", "HQ", "SC",
#'  and "FPE".
#' @param trans A transformation to apply to each criteria result (vector). Can
#'  be a function, "none" (the default), or "index" to create index numbers.
#' @eval roxy$args_gg("geom_line")
#' @eval roxy$dots("select", "vars::VARselect")
#'
#' @details
#' `r roxy$details_custom()`
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
    args_line = list(),
    ...) {
  select_test(series, lag.max, type, criteria, trans)

  criteria <- paste0(criteria, "(n)")

  setup <- select_setup(x, series, lag.max, type, criteria, trans, ...)

  ggplot(setup$data, aes(.data$lag, .data$value, color = .data$name)) +
    inject(ggplot2::geom_line(!!!args_line)) +
    ggplot2::labs(
      title = "Information Criteria for Each Lag", color = "Criteria",
      x = "Lag", y = "value"
    )
}

#' @noRd 
select_setup.list <- function(x, series, lag.max, type, criteria, trans, ...) {
  data <- select_helpers$format(x, criteria, trans)

  list(data = data)
}

#' @noRd 
select_setup.default <- function(x, series, lag.max, type, criteria, trans,
  ...) {
  series <- series %||% colnames(x)

  x <- vars::VARselect(x[,series], lag.max, type, ...)

  data <- select_helpers$format(x, criteria, trans)

  list(data = data)
}
