param_series <- function() {
  glue(
    "@param series A character vector with series (column names) to
    consider. Defaults to all (\\code{NULL})."
  )
}


param_index <- function(len) {
  paste0(
    "@param index A vector of labels to the x-axis, normally dates. Must have ",
    " length equal to ", len, ". Defaults to a numeric sequence."
  )
}


param_geom <- function(fun_names) {
  params <- gsub("geom_(.+)", "\\1", fun_names)

  texts <- c()
  for (i in seq_along(params)) {
    texts[[i]] <- paste0(
      "'", params[i], "'", if (i == 1) " (the default)",
      " for \\link[ggplot2]{", fun_names[i], "}"
    )
  }

  paste0(
    "@param geom The ggplot geom used to create the plot, ",
    paste0(texts, collapse = ", "), "."
  )
}


param_args_geom <- function() {
  "@param args_geom Arguments passed to the chosen \\code{geom}."
}


param_facet <- function(value) {
  paste0(
    "@param facet The facet \"engine\" to be used. 'ggplot2' for ",
    "\\link[ggplot2]{facet_grid}, 'ggh4x' for \\link[ggh4x]{facet_grid2}"
  )
}


param_args <- function(fun_name) {
  param <- ifelse(grepl("facet", fun_name),
    "args_facet", gsub("geom_(.+)", "args_\\1", fun_name)
  )

  paste0(
    "@param ", param,
    " Aditional arguments passed to \\link[ggplot2]{",
    fun_name, "}."
  )
}


param_colors <- function() {
  paste0(
    "@param colors A vector of colors for each variable. Passed to",
    "\\link[ggplot2]{scale_color_manual}. See \\code{vignette(\"colors\")}."
  )
}


param_linetypes <- function() {
  paste0(
    "@param linetypes A vector of line types (original, predicted). Passed to",
    "\\link[ggplot2]{scale_linetype_manual}."
  )
}


param_dots <- function(fun_name) {
  fun <- strsplit(fun_name, "::")[[1]]
  paste0(
    "@param ... Additional arguments passed to \\link[",
    fun[1], "]{", fun[2], "}."
  )
}
