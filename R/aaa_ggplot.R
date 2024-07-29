# Defining facets:
define_facet_grid <- function(facet_type, args_facet, env) {
  if (facet_type == "ggh4x") {
    tryCatch(check_installed("ggh4x"),
      error = function(e) {
        cli_warn(
          "Coercing `facet` to `'ggplot'`.",
          call = env
        )
        facet_type <- "ggplot"
      }
    )
  }

  inject(if (facet_type == "ggh4x") {
    list(ggh4x::facet_grid2(vars(.data$facet_x), vars(.data$facet_y),
      !!!args_facet
    ))
  } else if (facet_type == "ggplot") {
    list(facet_grid(vars(.data$facet_x), vars(.data$facet_y), !!!args_facet))
  })
}


# Define aesthetics:
define_aes <- function(args_aes, mapto) {
  purrr::map(args_aes, ~enexpr(mapto))
}

define_scales <- function(args_aes) {
  purrr::imap(args_aes, ~do_call(glue("ggplot2::scale_{.y}_manual"), .x))
}


# Define other:
define_sec_axis <- function(x_lab, y_lab) {
  my_sec_axis <- function(name) {
    ggplot2::sec_axis(~., name = name, breaks = NULL, labels = NULL)
  }

  list(
    ggplot2::scale_x_continuous(sec.axis = my_sec_axis(x_lab)),
    ggplot2::scale_y_continuous(sec.axis = my_sec_axis(y_lab)),
    ggplot2::theme(
      axis.title.x.top = ggplot2::element_text(vjust = 1.5),
      axis.title.y.right = ggplot2::element_text(vjust = 1.5)
    )
  )
}


# Updating labs and palette:
update_labs <- function(args_labs, new_labs) {
  args_labs[names(new_labs)] <- purrr::imap(new_labs, ~args_labs[[.y]] %||% .x)
}

update_values <- function(args_aes, graph_type, name, env) {
  suggested_aes <- list(
    bar = c("fill", "color", "alpha"),
    area = c("fill", "color", "alpha"),
    line = c("color", "linetype", "linewidth", "alpha"),
    segment = c("color", "linetype", "linewidth", "alpha")
  )[[graph_type]]

  if (purrr::none(args_aes, ~"values" %in% names(.x))) {
    cli_warn(
      "
      Some aesthetic must have its 'values' defined in {.var args_aes}. \\
      Setting {.code args_aes${suggested_aes[1]}$values} to {.val 'ggplot'}.
      ",
      call = env
    )
    args_aes[[suggested_aes[1]]]$values <- "ggplot"
  } else if (purrr::none(args_aes, ~"values" %in% suggested_aes)) {
    cli_warn(
      "
      This type of graph works best with the aesthetics {.or suggested_aes}, \\
      but none of them were defined. Consider using one of them instead.
      ",
      call = env
    )
  }

  if (is_null(args_aes[[suggested_aes[1]]]$name)) {
    args_aes[[suggested_aes[1]]]$name <- name
  }

  args_aes
}

process_values <- function(args_aes, n, env) {
  for (i in c("color", "fill")) {
    args_aes[[i]]$values <- get_palette(args_aes[[i]]$values, n, env)
  }
  args_aes
}

get_palette <- function(x, n, env) {
  if (is_null(x)) {
    return(NULL)
  }

  if (x == "ggplot") {
    values <- grDevices::hcl(
      h = seq(15, 375, length = n + 1), l = 65, c = 100
    )[1:n]
    return(values)
  }

  if (is_bare_character(x, 1) && grepl("::", x)) {
    pkg_pal <- strsplit(x, "::")[[1]]
    check_installed(pkg_pal[1], call = env)
    supported <- supported_palettes[[pkg_pal[1]]]

    if (!pkg_pal[2] %in% supported) {
      cli_abort(
        "
        With package {.val {pkg_pal[1]}}, palette should be one of: \\
        {.or {supported}}.
        ",
        call = env
      )
    }

    if (pkg_pal[1] == "RColorBrewer") {
      values <- RColorBrewer::brewer.pal(n, pkg_pal[2])
    } else if (pkg_pal[1] %in% c("base", "viridis", "ggsci")) {
      values <- do_call(x, list(n))[1:n]
    }

    return(values)
  }

  if (is_bare_character(colors)) {
    return(colors)
  }

  cli_abort(
    "Unrecognized {.val 'args_aes$colors'} argument",
    call = env
  )
}
