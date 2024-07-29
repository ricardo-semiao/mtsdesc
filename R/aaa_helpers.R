utils::globalVariables(".") # Remove CRAN note towards the magrittr dot


# Color Helpers:
color_supported <- list(
  base = c(
    "rainbow", "heat.colors", "terrain.colors",
    "topo.colors", "cm.colors"
  ),
  viridis = c(
    "viridis", "magma", "inferno", "plasma",
    "cividis", "mako", "rocket", "turbo"
  ),
  RColorBrewer = c(
    "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
    "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
    "YlOrBr", "YlOrRd", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
    "Set1", "Set2", "Set3", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
    "RdYlBu", "RdYlGn", "Spectral"
  ),
  ggsci = c(
    "pal_npg", "pal_aaas", "pal_nejm", "pal_lancet", "pal_jama", "pal_jco",
    "pal_ucscgb", "pal_d3", "pal_locuszoom", "pal_igv", "pal_cosmic",
    "pal_uchicago", "pal_startrek", "pal_tron", "pal_futurama",
    "pal_rickandmorty", "pal_simpsons", "pal_flatui", "pal_frontiers",
    "pal_gsea", "pal_material"
  )
)


# Other helpers:
ignore_cols <- function(arg, env) {
  isnumeric_cols <- sapply(arg, \(x) is_integer(x) || is_double(x))

  if (all(isnumeric_cols)) {
    arg
  } else {
    cli::cli_warn(
      "Ignoring non numeric columns (or equivalent) in {.var x}.",
      call = env
    )
    arg[, isnumeric_cols]
  }
}

get_series <- function(series, series_all, env) {
  arg_name <- ensym(series)
  if (is_null(series)) {
    series_all
  } else {
    if (!all(series %in% series_all)) {
      cli::cli_warn(
        "Not all elements of {.var {arg_name}} are present in {.var x}.",
        call = env
      )
    }
    series
  }
}

do_call <- function(fun_name, args) {
  x <- strsplit(fun_name, "::|:::")[[1]]
  do.call(get(x[2], envir = loadNamespace(x[1])), args)
}
