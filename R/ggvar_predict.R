# Helper functions used between more than one method:
predict_helpers <- list()

predict_helpers$format <- function(x, series, index_ahead, index_behind, ci, ...) {
  pred <- stats::predict(x, n.ahead = length(index_ahead), ci = ci, ...) %>%
    `[[`("fcst") %>%
    `[`(series) %>%
    purrr::imap_dfr(function(prediction, name) {
      tibble::as_tibble(prediction) %>%
        dplyr::mutate(serie = name, index = index_ahead, type = "prediction")
    })

  if (!is_null(index_behind)) {
    orig <- x$datamat[(x$obs - length(index_behind) + 1):x$obs, 1:x$K] %>%
      dplyr::mutate(index = index_behind, type = "original") %>%
      tidyr::pivot_longer(-c("index", "type"),
        names_to = "serie", values_to = "fcst"
      )

    dplyr::bind_rows(orig, pred)
  } else {
    pred
  }
}


# Startup tests and setup function to get data from `x` (methods at the end):
#' @noRd
predict_test <- function(
    series, index_ahead, index_behind, ci, env) {
  test$type(series, c("NULL", "character"), env)
  test$type(index_ahead, c("NULL", "integer", "double"), env)
  test$type(index_behind, c("NULL", "integer", "double"), env)
  test$interval(ci, 0, 1, FALSE, env)
}

#' @noRd
predict_setup <- function(
    x, series, index_ahead, index_behind, ci, ..., env) {
  UseMethod("predict_setup")
}


#' Plot VAR predictions
#'
#' Gets predictions via [stats::predict] and plots them. Can also plot the known
#'  values of the past.
#'
#' @param x A "varest" object to get predictions from.
#' @eval roxy$series()
#' @param index_ahead A vector of labels to the x-axis, normally dates. Applied
#'  to the predicted portion of the graph. Its length will define the
#'  prediction horizon.
#' @param index_behind A vector of labels to the x-axis, normally dates. Applied
#'  to the original portion of the graph. Its length will define the 'past'
#'  horizon. Leave as `NULL` to only plot predicted values.
#' @eval roxy$ci("stats::predict")
#' @eval roxy$args_aes()
#' @eval roxy$args_geom(c("geom_line", "geom_ribbon"))
#' @eval roxy$args_labs()
#' @eval roxy$args_facet()
#' @eval roxy$dots()
#'
#' @details
#' `r roxy$details_custom(TRUE)`
#' `r roxy$details_methods()$predict`
#'
#' @eval roxy$return_gg()
#'
#' @eval roxy$fam_hist()
#'
#' @examples
#' x <- vars::VAR(freeny[-2])
#' ggvar_predict(x, NULL, 1:10, 0:-10, args_facet = list(scales = "free_y"))
#'
#' @export
ggvar_predict <- function(
    x, series = NULL, index_ahead, index_behind = NULL,
    ci = 0.95,
    args_aes = list(),
    args_line = list(),
    args_ribbon = list(fill = NA, linetype = 2, color = "blue"),
    args_labs = list(),
    args_facet = list(),
    ...) {
  # Test and setup:
  env <- current_env()

  predict_test(series, index_ahead, index_behind, ci, env = env)
  setup <- predict_setup(x, series, index_ahead, index_behind, ci, ..., env = env)

  # Update arguments:
  args_labs <- update_labs(args_labs, list(
    title = "VAR Predicted Values", x = "Index", y = "Values"
  ))

  args_aes <- update_values(args_aes, "line", "Type", env = env) %>%
    process_values(2, env = env)

  # Create additions:
  add_ribbon <- inject(if (!is_false(ci)) {
    list(
      geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), !!!args_ribbon)
    )
  })

  add_aes <- define_aes(args_aes, .data$serie)

  # Graph:
  inject(
    ggplot(setup$data, aes(.data$index, .data$fcst, !!!add_aes)) +
      add_ribbon +
      geom_line(!!!args_line) +
      facet_wrap(vars(.data$serie), !!!args_facet) +
      labs(!!!args_labs)
  )
}


# Setup methods:
#' @noRd
predict_setup.varest <- function(
    x, series, index_ahead, index_behind, ci, ..., env) {
  series <- get_series(series, names(x$varresult), env)

  data <- predict_helpers$format(x, series, index_ahead, index_behind, ci, ...)

  list(data = data)
}
