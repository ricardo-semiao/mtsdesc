# Helpers:
gg_info_helpers <- list()

gg_info_helpers$get_mappings <- function(layer) {
  list(
    mappings = layer$computed_mapping,
    data = `if`(inherits_only(layer$data, "waiver"), "inherited", layer$data)
  )
}

gg_info_helpers$get_funnames <- function(layers) {
  purrr::map(layers, function(layer) {
    class(layer$geom)[1] %>%
      gsub("^([A-Z][a-z]+)([A-Z][a-z]+)$", "\\1_\\2", .) %>%
      tolower()
  })
}


#' Find build information of a ggplot
#'
#' To customize a plot made in a package, adding new layers, it is useful to
#'  see the data and mappings used. This function returns a list with both.
#'
#' @param graph A ggplot to analyze.
#'
#' @return A list with two entries:
#'  * `main_data`: the data used in `ggplot()`.
#'  * `layers`: for each layer the aesthetic mapping and the data used (which
#'  can be `'inherited'`).
#'
#' @examples
#' g <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
#'   ggplot2::geom_point(ggplot2::aes(color = gear))
#' find_gg_info(g)
#'
#' @export
find_gg_info <- function(graph) {
  info <- list()

  info$main_data = tibble::as_tibble(graph$data)

  info$layers = purrr::map(graph$layers, gg_info_helpers$get_mappings) %>%
    stats::setNames(gg_info_helpers$get_funnames(graph$layers))

  info$facets = names(graph$facet$params$facets)

  info
}
