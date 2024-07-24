#' @noRd
setup_ggvar_predict <- function(x, n.ahead, data_test, series, index, ci) {
  test$class_arg(x, c("varest", "varprd"))
  test$class_arg(data_test, c("data.frame", "matrix", "NULL"))
  data_test <- test$dataset_arg(data_test)
  test$series(series, x)
  test$interval_arg(ci, 0, 1, FALSE)

  if (inherits(x, "varprd")) {
    if (!is.null(n.ahead)) warning("`x` of class 'varprd', ignoring `n.ahead`")
    n.ahead <- nrow(x$fcst[[1]])
  } else {
    msg <- "`n.ahead` must be supplied with `x` of class 'varest' and `data_test = NULL`"
    n.ahead <- n.ahead %||% nrow(data_test) %||% stop(msg)
  }

  if (!is.null(data_test) && nrow(data_test) != n.ahead) {
    stop("`data_test` with incorrect number of rows")
  }

  index <- index %||% 1:n.ahead
  test$index(index, n = n.ahead)

  list(
    n.ahead = n.ahead,
    data_test = data_test,
    series = series %||% get_names(x),
    index = index,
    guide = if (is.null(data_test)) "none" else "legend"
  )
}

#' Plot the Predicted Values of a VAR
#'
#' Plots the result of a [predict.varest][vars::predict.varest] call. Has an option to
#'  overlay it with the true variables, if provided a test dataset.
#'
#' @param x A "varest" object to get predictions from, or, directly, a
#'  "varprd" object.
#' @param data_test A test data set (object coercible to data.frame), with the
#'  actual series values. If `NULL`, no comparison is made.
#' @param n.ahead An integer. The number of periods to predict, passed to
#'  [predict][stats::predict]. Defaults to `nrow(data_test)` or the horizon of
#'  the "varprd" object (`NULL`).
#' @eval roxy$series()
#' @eval roxy$index("`n.ahead`")
#' @param ci The level of confidence for the prediction confidence interval. Set
#'  to `FALSE` to omit. Passed to [predict][stats::predict].
#' @param ... Additional arguments passed to [predict][stats::predict].
#' @eval roxy$linetypes()
#' @eval roxy$args_gg(c("geom_line", "geom_ribbon", "facet_wrap"))
#'
#' @return An object of class `ggplot`.
#'
#' @examples
#' ggvar_predict(stats::predict(vars::VAR(freeny[-2])),
#'   args_facet = list(scales = "free_y")
#' )
#' ggvar_predict(vars::VAR(freeny[-2][1:30, ]), freeny[-2][31:39, ],
#'   args_facet = list(scales = "free_y")
#' )
#'
#' @export
ggvar_predict <- function(
    x, data_test = NULL, n.ahead = NULL, series = NULL, index = NULL,
    ci = 0.95, ...,
    linetypes = c("solid", "dashed"),
    args_line = list(),
    args_ribbon = list(fill = NA, linetype = 2, color = "blue"),
    args_facet = list()) {
  # Setup:
  setup <- setup_ggvar_predict(x, n.ahead, data_test, series, index, ci)
  reassign <- c("n.ahead", "data_test", "series", "index")
  list2env(setup[reassign], envir = rlang::current_env())

  # Data:
  pred <- if (inherits(x, "varest")) {
    stats::predict(x, n.ahead = n.ahead, ci = ci, ...)
  } else {
    x
  }

  data_pred <- pred$fcst[series] %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    dplyr::mutate(
      serie = rep(series, each = n.ahead),
      index = rep(index, length(series))
    ) %>%
    dplyr::rename(prediction = "fcst")

  data_pred <- if (is.null(data_test)) {
    data_pred %>%
      tidyr::pivot_longer(c("prediction"),
        values_to = "value", names_to = "type"
      )
  } else {
    data_test %>%
      as.data.frame() %>%
      dplyr::select(dplyr::all_of(series)) %>%
      dplyr::mutate(index = index) %>%
      tidyr::pivot_longer(-c("index"),
        values_to = "actual", names_to = "serie"
      ) %>%
      dplyr::full_join(data_pred, by = c("index", "serie")) %>%
      tidyr::pivot_longer(c("prediction", "actual"),
        values_to = "value",
        names_to = "type"
      )
  }

  # Graph:
  ggplot_add <- list(
    if (!isFALSE(ci)) {
      inject(ggplot2::geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper),
        !!!args_ribbon
      ))
    }
  )

  ggplot(data_pred, aes(.data$index, .data$value)) +
    ggplot_add +
    inject(ggplot2::geom_line(aes(linetype = .data$type), !!!args_line)) +
    inject(ggplot2::facet_wrap(vars(.data$serie), !!!args_facet)) +
    ggplot2::scale_linetype_manual(values = linetypes, guide = setup$guide) +
    ggplot2::labs(
      title = "VAR Predicted Values", x = "Index",
      y = "Values", linetypes = "Type"
    )
}
