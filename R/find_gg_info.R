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
#' g <- ggplot(mtcars, aes(mpg, hp)) + geom_point(aes(color = gear))
#' find_gg_info(g)
#' 
#' @export
find_gg_info <- function(graph) {
  get_mappings <- function(layer) {
    list(
      mappings = layer$computed_mapping,
      data = `if`(inherits_only(layer$data, "waiver"), "inherited", layer$data)
    )
  }
  get_layer_names <- function(layers) {
    purrr::map(layers, ~as_string(.x$constructor[[1]]))
  }

  list(
    main_data = tibble::as_tibble(graph$data),
    layers = purrr::map(graph$layers, get_mappings) %>%
      setNames(get_layer_names(graph$layers))
  )
}
