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
# knitr::kable(sapply(suported_palettes, \(x) paste(x, collapse = ", ")))

get_pallete <- function(palette, n, ...) {
  if (is.null(palette)) {
    return(grDevices::hcl(
      h = seq(15, 375, length = n + 1), l = 65, c = 100
    )[1:n])
  }

  if (length(palette) == 1 && grepl("::", palette)) {
    pkg_pal <- strsplit(palette, "::")[[1]]

    if (!rlang::is_installed(pkg_pal[1])) {
      stop(paste0("Package ", pkg_pal[1], " isn't installed"))
    }
    if (!pkg_pal[2] %in% suported_palettes[[pkg_pal[1]]]) {
      stop(paste0(
        "With package ", pkg_pal[1], ", palette should be one of:\n    ",
        suported_palettes[[pkg_pal[1]]]
      ))
    }

    if (pkg_pal[1] == "ggplot2") {
      return(pkg_pal[2])
    } else if (pkg_pal[1] == "RColorBrewer") {
      return(RColorBrewer::brewer.pal(n, pkg_pal[2], ...))
    } else if (pkg_pal[1] %in% c("base", "viridis", "ggsci")) {
      return(match.fun(palette)(n, ...)[1:n])
    }
  }

  if (is.atomic(palette)) {
    return(palette)
  }

  stop("Unrecognized `palette` argument")
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



# Test Helpers ------------------------------------------------------------

