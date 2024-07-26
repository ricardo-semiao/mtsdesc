# Seed and variables for the tests:
#set.seed(91127)
#
#args <- list(
#  df = list(
#    x = list(df = "freeny[-2]"),
#    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
#    geom = list(seg = "'segment'", area = "'area'")
#  ),
#  mts = list(
#    x = list(mts = "EuStockMarkets"),
#    series = list(null = "NULL", single = "'DAX'", multi = c('DAX', 'SMI')),
#    geom = list(seg = "'segment'", area = "'area'")
#  ),
#  var = list(
#    x = list(var = "vars::VAR(freeny[-2])"),
#    series = list(null = "NULL", single = "'y'", multi = c('y', 'price.index')),
#    geom = list(seg = "'segment'", area = "'area'")
#  )
#)
#
#example <- ggvar_ccf(vars::VAR(freeny[-2]), ci = FALSE, lag.max = 9)
#
## Tests:
#test_combinations("ggvar_ccf", args$df, "x=dataframe")
#test_combinations("ggvar_ccf", args$mts, "x=mts")
#test_combinations("ggvar_ccf", args$var, "x=varest")
#
#test_that("'external' args combinations work", {
#  expect_no_error({
#    stopifnot(
#      "`lag.max` arg didn't worked" =
#        all(unique(example$data$lag) == 0:9)
#    )
#    stopifnot(
#      "`ci = FALSE` arg didn't worked" =
#        all(sapply(example$layers, \(x) !inherits(class(x$geom), "GeomRibbon")))
#    )
#  })
#  expect_doppelganger("external", ggvar_ccf(freeny[-2],
#    args_facet = list(scales = "free_y", independent = "y"),
#    args_hline = list(color = "red"),
#    args_type = list(alpha = 0.5),
#    facet = "ggh4x", ci = 0.5
#  ))
#})
#
#set.seed(NULL)
