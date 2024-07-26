utils::globalVariables(".") # Remove CRAN note towards the magrittr dot



# Palette Helpers ---------------------------------------------------------

suported_palettes <- list(
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

get_colors <- function(colors, n, ..., evn = caller_env()) {
  if (is_null(colors)) {
    grDevices::hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
  } else if (length(colors) == 1 && grepl("::", colors)) {
    pkg_pal <- strsplit(colors, "::")[[1]]

    if (!rlang::is_installed(pkg_pal[1])) {
      cli::cli_abort("Package {.val {pkg_pal[1]}} isn't installed.", call = env)
    } else if (!pkg_pal[2] %in% suported_palettes[[pkg_pal[1]]]) {
      cli::cli_abort(
        "With package {.val {pkg_pal[1]}}, palette should be one of: 
        {.or {suported_palettes[[pkg_pal[1]]]}}."
      )
    }

    if (pkg_pal[1] == "ggplot2") {
      pkg_pal[2]
    } else if (pkg_pal[1] == "RColorBrewer") {
      RColorBrewer::brewer.pal(n, pkg_pal[2], ...)
    } else if (pkg_pal[1] %in% c("base", "viridis", "ggsci")) {
      match.fun(colors)(n, ...)[1:n]
    }
  } else if (is_bare_character(colors)) {
    colors
  } else {
    cli::cli_abort("Unrecognized {.val 'colors'} argument")
  }
}



# Ggplot Helpers ----------------------------------------------------------

create_sec_axis <- function(xseclab = "Impulse", yseclab = "Response") {
  my_sec_axis <- function(name) {
    ggplot2::sec_axis(~., name = name, breaks = NULL, labels = NULL)
  }

  list(
    ggplot2::scale_x_continuous(sec.axis = my_sec_axis(xseclab)),
    ggplot2::scale_y_continuous(sec.axis = my_sec_axis(yseclab)),
    ggplot2::theme(
      axis.title.x.top = ggplot2::element_text(vjust = 1.5),
      axis.title.y.right = ggplot2::element_text(vjust = 1.5)
    )
  )
}

define_facet <- function(facet, facet_x, facet_y, ...) {
  if (facet == "ggplot") {
    ggplot2::facet_grid(stats::reformulate(facet_x, facet_y), ...)
  } else if (facet == "ggh4x") {
    if (!is_installed("ggh4x")) {
      cli::cli_warn("
      Package ggh4x is not installed. Coercing `facet` to `'ggplot'`.
      ")
      ggplot2::facet_grid(stats::reformulate(facet_x, facet_y), ...)
    } else {
      ggh4x::facet_grid2(stats::reformulate(facet_x, facet_y), ...)
    }
  }
}



# Others  ---------------------------------------------------------------

ignore_cols <- function(arg) {
  isnumeric_cols <- sapply(arg, \(x) is_integer(x) || is_double(x))

  if (all(isnumeric_cols)) {
    arg
  } else {
    cli::cli_warn("Ignoring non numeric columns (or equivalent) in {.var x}.")
    arg[, isnumeric_cols]
  }
}

get_series <- function(series, series_all, env) {
  arg_name <- ensym(series)
  if (is_null(series)) {
    series_all
  } else {
    if (!all(series %in% series_all)) {
      cli::cli_warn("
      Not all elements of {.var {arg_name}} are present in {.var x}.
      ",
        call = env
      )
    }
    series
  }
}
