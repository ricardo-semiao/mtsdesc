pluralize_or <- function(texts_vec) {
  pluralize_and <- utils::capture.output(cli::pluralize("{texts_vec}"))
  gsub("and", "or", pluralize_and)
}


roxy <- list()


# Series and Index arguments:
roxy$series <- function() {
  glue("
  @param series A character vector with series (variables) to  consider. \\
  Defaults to all (`NULL`).
  ")
}

roxy$index <- function(lengths_vec) {
  text <- glue("`{lengths_vec}`") %>% pluralize_or()
  glue("
  @param index A vector of labels to the x-axis, normally dates. Must have \\
  length equal to {text}. Defaults to a integer sequence.
  ")
}


# ggplot-related arguments:
roxy$graph_type <- function(types_vec, isgeoms = TRUE) {
  if (isgeoms) {
    texts <- purrr::map_chr(types_vec,
      ~glue("`'{.x}'`, for [ggplot2::geom_{.x}]")
    ) %>%
      pluralize_or()
    glue("
    @param graph_type The ggplot geom used to create the plot: {texts}.
    ")
  } else {
    texts <- glue({"`'{types_vec}'`"}) %>% pluralize_or()
    glue("
    @param graph_type The type of the plot: {texts}.
    ")
  }
}

roxy$facet_type <- function() {
  glue("
  @param facet_type The facet 'engine' to be used. `'ggplot2'` for \\
  [ggplot2::facet_grid], `'ggh4x'` for [ggh4x::facet_grid2].
  ")
}

roxy$args_gg <- function(funs_vec) {
  param_name <- ifelse(
    grepl("facet", funs_vec),
    "args_facet",
    gsub("geom_(.+)", "args_\\1", funs_vec)
  )

  text <- ifelse(
    grepl("facet", funs_vec),
    "the faceting engine used",
    glue("[ggplot2::{funs_vec}]")
  )

  glue("
  @param {param_name} Additional arguments passed to {text}.
  ")
}

roxy$args_type <- function() {
  "@param args_type Arguments passed to the 'geom' chosen in `graph_type`."
}


# Other arguments
roxy$dots <- function(funs_vec, special_method = NULL) {
  funs_text <- glue("varr:::{funs_vec}_setup") %>% pluralize_or()

  special_text <- if (!is_null(special_method)) {
    glue("Pass additional arguments to [{special_method}] here. ")
  } else {
    ""
  }

  glue("
  @param ... Arguments passed to `{funs_text}`, the generic \\
  function that formats `x` into a 'graphable' format. {special_text}Use them \\
  if you have created a method for some unsupported class of `x`.
  ")
}

roxy$ci <- function(fun) {
  glue("
  @param ci The level of confidence for the prediction confidence interval. \\
  Set to `FALSE` to omit. Passed to [{fun}].
  ")
}


# Other documentations
roxy$return_gg <- function() {
  glue("@return A [ggplot][ggplot2::ggplot].")

}

roxy$details_custom <- function() {
  glue("
  ## Customization
  The graph can be customized both with the 'static' arguments passed to each \\
  layer -- using the `args_*` arguments --, and, if applicable, the 'dynamic' \\
  aesthetics -- using the `args_aes` argument.

  After built, the result can be further customized as any ggplot, adding or \\
  overwriting layers with the [ggplot's +][ggplot2::+.gg]. It is useful to \\
  understand the data and the mappings coded by the package, using the \\
  function [find_gg_info].

  See [vignette('customizing-graphs')] for more details.
  ")
}

roxy$details_methods <- function() {
  get_text <- function(class_vec, funs_vec) {
    funs_text <- ifelse(funs_vec == "none", "nothing", glue("[{funs_vec}]"))
    methods_text <- glue("* Class `'{class_vec}'`: passed to {funs_text}.") %>%
    glue::glue_collapse(sep = "\n")

    glue("
    ## Methods
    Each class of `x` conditions a function to extract its data from, to \\
    which the `...` arguments will be passed. Below there is a list with all \\
    the currently implemented classes, and the relevant external documentations:
    {methods_text}
    ")
  }

  list(
    acf = c("varest" = "stats::acf", "default" = "stats::acf"),
    fevd = c("varest" = "vars::fevd"),
    stability = c("varest" = "vars::stability"),
    select = c("list" = "none", "default" = "vars::VARselect"),
    irf = c("varest" = "vars::irf", "varirf" = "none"),
    predict = c("varest" = "vars::predict.varest")
  ) %>%
    purrr::map(~get_text(names(.x), .x))
}


# Families
roxy$fam_ts <- function() {
  "@family general time series plots"
}
roxy$fam_diag <- function() {
  "@family model diagnostics plots"
}
roxy$fam_hist <- function() {
  "@family historic values plots"
}
roxy$fam_output <- function() {
  "@family VAR output plots"
}
