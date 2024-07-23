pluralize_or <- function(text) {
  pluralize_and <- utils::capture.output(cli::pluralize("{text}"))
  gsub("and", "or", pluralize_and)
}


param_series <- function() {
  glue("
  @param series A character vector with series (column names) to \\
  consider. Defaults to all (\\code{{NULL}}).
  ")
}

param_index <- function(len) {
  glue("
  @param index A vector of labels to the x-axis, normally dates. Must have \\
  length equal to {len}. Defaults to a integer sequence.
  ")
}

param_graph_type <- function(types, isgeoms = TRUE) {
  if (isgeoms) {
    texts <- purrr::map_chr(types, ~glue("
      \\code{{'{.x}'}}, for \\link[ggplot2]{{geom_{.x}}}
      ")) %>%
      pluralize_or()
    glue("
    @param graph_type The ggplot geom used to create the plot: {texts}.
    ")
  } else {
    texts <- glue({"\\code{{'{types}'}}"}) %>% pluralize_or()
    glue("
    @param graph_type The type of the plot, {texts}.
    ")
  }
}

param_facet_type <- function() {
  glue("
  @param facet The facet 'engine' to be used. \\code{{'ggplot2'}} for \\
  \\link[ggplot2]{{facet_grid}}, \\code{{'ggh4x'}} for \\
  \\link[ggh4x]{{facet_grid2}}
  ")
}


param_colors <- function() {
  glue("
  @param colors A vector of colors for each variable. Passed to \\
  \\link[ggplot2]{{scale_color_manual}}. See \\code{{vignette('colors')}}.
  ")
}


param_args <- function(fun_name) {
  param <- ifelse(
    grepl("facet", fun_name),
    "args_facet",
    gsub("geom_(.+)", "args_\\1", fun_name)
  )

  glue("
  @param {param} Additional arguments passed to \\
  \\link[ggplot2]{{{fun_name}}}.
  ")
}

param_args_geom <- function() {
  "@param args_geom Arguments passed to the chosen \\code{{geom}}."
}


param_linetypes <- function() {
  glue("
  @param linetypes A vector of line types (original, predicted). Passed to \\
  \\link[ggplot2]{{scale_linetype_manual}}.
  ")
}

param_dots <- function(fun_names) {
  fun_names <- paste0("varr:::", fun_names) %>% pluralize_or()
  glue("
  @param ... Arguments passed to \\code{{{fun_names}}}, the generic \\
  function that formats \\code{{x}} into a 'graphable' format. Use them only \\
  if you have created a method for some unsupported class of \\code{{x}}.
  ")
}
