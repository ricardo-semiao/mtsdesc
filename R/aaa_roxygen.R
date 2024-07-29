pluralize_and <- function(texts_vec) {
  utils::capture.output(cli::pluralize("{texts_vec}"))
}

pluralize_or <- function(texts_vec) {
  gsub("and", "or", pluralize_and("{texts_vec}"))
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


# Facet arguments:
roxy$faceted <- function() {
  glue("
  @param faceted Should the graph be divided in facets?.
  ")
}

roxy$facet_type <- function() {
  glue("
  @param facet_type The facet 'engine' to be used. `'ggplot2'` for \\
  [ggplot2::facet_grid], `'ggh4x'` for [ggh4x::facet_grid2].
  ")
}

roxy$args_facet <- function() {
  glue("
  @param args_facet Additional arguments passed to the faceting engine used.
  ")
}


# gg function arguments:
roxy$args_aes <- function() {
  glue("
  @param args_aes A named list defining aesthetics to differentiate the data \\
  by, an the arguments passed to `ggplot2::scale_*_manual`. See more in the \\
  'Customization' section.
  ")
}

roxy$args_labs <- function() {
  glue("
  @param args_labs Additional arguments passed to [labs][ggplot2::labs]. If \\
  an empty list, will be changed to default values.
  ")
}

roxy$args_geom <- function(funs_vec) {
  param_name <- gsub("geom_(.+)", "args_\\1", funs_vec) %>%
    paste0(collapse = ",")
  text <- glue("[{funs_vec}][ggplot2::{funs_vec}]") %>%
    pluralize_and()

  glue("
  @param {param_name} Additional arguments passed to {text} (respectively). \\
  See more in the 'Customization' section.
  ")
}


# Graph type and its geom arguments
roxy$graph_type <- function(types_vec, isgeoms = TRUE) {
  texts <- purrr::map_chr(types_vec, \(x) {
    glue("`'{.x}'`, for [geom_{.x}][ggplot2::geom_{.x}]")
  }) %>%
    pluralize_or()

  glue("
  @param graph_type The ggplot geom used to create the plot: {texts}.
  ")
}

roxy$args_type <- function() {
  "@param args_type Arguments passed to the 'geom' chosen in `graph_type`."
}


# Other arguments
roxy$dots <- function() {
  glue("
  @param ... Arguments passed to methods, see the 'Methods' section.
  ")
}

roxy$ci <- function(fun) {
  text_short <- strsplit(fun, "::")[[1]][2]
  glue("
  @param ci The confidence level for the confidence interval. Set to `FALSE` \\
  to omit. Used in [{text_short}][{fun}].
  ")
}

roxy$unused <- function(class) {
  glue("
  Unused if `x` is of class {class}.
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

  The `args_aes` is a list with '* = arguments to `scale_*_manual` elements, \\
  where '*' represents the name of an aesthetic to apply to the data. View \\
  [vignette('ggplot2-specs', 'ggplot2')] to see the available aesthetics.

  After built, the result can be further customized as any ggplot, adding or \\
  overwriting layers with the [ggplot's +][ggplot2::+.gg]. It is useful to \\
  understand the data and the mappings coded by the package, using the \\
  function [find_gg_info].

  See [vignette('customizing-graphs')] for more details.
  ")
}

roxy$details_methods <- function() {
  get_text <- function(topics_vec, class_vec, funs_vec) {
    topic_text <- glue("varr:::{topics_vec}_setup") %>% pluralize_or()

    funs_text <- ifelse(funs_vec == "none", "nothing", glue("[{funs_vec}]"))

    methods_text <- glue("* Class `'{class_vec}'`: passed to {funs_text}.") %>%
      glue::glue_collapse(sep = "\n")

    glue("
    ## Methods
    The data from `x` is extracted with the generic function {topic_text}. \\
    Each class conditions an external function to pass the `...` arguments \\
    to. Below there is a list with all the currently implemented classes:
    {methods_text}
    ")
  }

  list(
    acf = c("varest" = "stats::acf", "default" = "stats::acf"),
    dispersion = c("default" = "none"),
    distribution = c("varest" = "none", "default" = "none"),
    fevd = c("varest" = "vars::fevd", "varfevd" = "none"),
    stability = c("varest" = "vars::stability", "varstabil" = "none"),
    select = c("list" = "none", "default" = "vars::VARselect"),
    irf = c("varest" = "vars::irf", "varirf" = "none"),
    history = c("varest" = "none", "default" = "none"),
    predict = c("varest" = "vars::predict.varest")
  ) %>%
    purrr::imap(~get_text(
      `if`(.y == "acf", c("acf", "ccf"), .y),
      names(.x),
      .x
    ))
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
