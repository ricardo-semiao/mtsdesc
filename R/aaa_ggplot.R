# Defining facets and aes:
define_facet_grid <- function(facet_type, ..., env = caller_env()) {
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

  if (facet_type == "ggh4x") {
    ggh4x::facet_grid2(vars(.data$facet_x), vars(.data$facet_y), ...)
  } else if (facet_type == "ggplot") {
    facet_grid(vars(.data$facet_x), vars(.data$facet_y), ...)
  }
}

define_facet_wrap <- function(faceted, ...) {
  inject(list(
    if (!is_false(faceted)) {
      facet_wrap(vars(.data$serie), ...)
    }
  ))
}

define_aes <- function(aes_args, mapto) {
  purrr::map(aes_args, ~enquo(mapto))
}

define_scales <- function(aes_args) {
  purrr::imap(aes_args, ~do_call(glue("ggplot2::scale_{.y}_manual"), .x))
}



# Updating labs and palettes:
update_labs <- function(args_labs, new_labs) {
  args_labs[names(new_labs)] <- purrr::imap(new_labs, ~args_labs[[.y]] %||% .x)
}

update_palette <- function(x, n, env = caller_env()) {
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
    supported <- color_supported[[pkg_pal[1]]]

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


# Others:
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
