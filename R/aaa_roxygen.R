pluralize_or <- function(text) {
  pluralize_and <- utils::capture.output(cli::pluralize("{text}"))
  gsub("and", "or", pluralize_and)
}


roxy <- list()

roxy$series <- function() {
  glue("
  @param series A character vector with series (column names) to \\
  consider. Defaults to all (`NULL`).
  ")
}

roxy$index <- function(lens) {
  text <- glue("`{lens}`") %>% pluralize_or()
  glue("
  @param index A vector of labels to the x-axis, normally dates. Must have \\
  length equal to {text}. Defaults to a integer sequence.
  ")
}


roxy$graph_type <- function(types, isgeoms = TRUE) {
  if (isgeoms) {
    texts <- purrr::map_chr(types,
      ~glue("`'{.x}'`, for [ggplot2::geom_{.x}]")
    ) %>%
      pluralize_or()
    glue("
    @param graph_type The ggplot geom used to create the plot: {texts}.
    ")
  } else {
    texts <- glue({"`'{types}'`"}) %>% pluralize_or()
    glue("
    @param graph_type The type of the plot, {texts}.
    ")
  }
}

roxy$facet_type <- function() {
  glue("
  @param facet The facet 'engine' to be used. `'ggplot2'` for \\
  [ggplot2::facet_grid], `'ggh4x'` for [ggh4x::facet_grid2].
  ")
}

roxy$colors <- function() {
  glue("
  @param colors A vector of colors for the 'geoms' and/or variables.
  See [vignette('colors')].
  ")
}


roxy$args_gg <- function(fun_name) {
  param <- ifelse(
    grepl("facet", fun_name),
    "args_facet",
    gsub("geom_(.+)", "args_\\1", fun_name)
  )
  text <- ifelse(
    grepl("facet", fun_name),
    "the faceting engine used",
    glue("[ggplot2::{fun_name}]")
  )

  glue("
  @param {param} Additional arguments passed to {text}.
  ")
}

roxy$args_geom <- function() {
  "@param args_geom Arguments passed to the chosen 'geom'."
}


#roxy$linetypes <- function() {
#  glue("
#  @param linetypes A vector of line types (original, predicted). Passed to \\
#  \\link[ggplot2]{{scale_linetype_manual}}.
#  ")
#}


roxy$dots <- function(fun_names, special_method = NULL) {
  fun_names <- glue("varr:::{fun_names}_setup") %>% pluralize_or()

  special_text <- if (!is_null(special_method)) {
    glue("Pass additional arguments to [special_method] here.")
  } else {
    ""
  }

  glue("
  @param ... Arguments passed to `{fun_names}`, the generic \\
  function that formats `x` into a 'graphable' format. Use them only \\
  if you have created a method for some unsupported class of `x`.{special_text}
  ")
}


roxy$return_gg <- function() {
  "@return An object of class `'ggplot'`."
}
