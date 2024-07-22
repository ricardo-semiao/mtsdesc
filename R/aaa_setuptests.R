pluralize_or <- function(x) {
  pluralize_and <- utils::capture.output(cli::pluralize("{x}"))
  gsub("and", "or", pluralize_and)
}

get_available_methods <- function(f) {
  f_name <- as_string(ensym(f))
  utils::methods(f_name) %>%
    attr("info") %>%
    rownames() %>%
    gsub(paste0(f_name, "."), "", ., fixed = TRUE)
}


test <- list()

test$class_arg <- function(arg, classes) {
  arg_name <- rlang::ensym(arg)
  if (!inherits(arg, classes)) {
    stop(paste0(
      "`", arg_name, "` must inherit one of ",
      paste0("'", classes, "'", collapse = ", ")
    ))
  }
}

test$index <- function(index, n, required = NULL, env) {
  arg_name <- rlang::ensym(arg)
  required <- required %||% deparse(rlang::enexpr(n))

  if (anyDuplicated(index)) {
    cli::cli_abort("{.var index} musn't have duplicated entries.", call = env)
  }
  if (length(index) != n) {
    cli::cli_abort("{.var index} must have a length of {required}.", call = env)
  }
}

test$categorical <- function(arg, options, env) {
  arg_name <- rlang::ensym(arg)

  if (!arg %in% options) {
    cli::abort("{.var {arg_name}} must be {pluralize_or(options)}", call = env)
  }
}

test$boolean_arg <- function(arg) {
  arg_name <- rlang::ensym(arg)
  if (!(isTRUE(arg) || isFALSE(arg))) {
    stop(paste0("`", arg_name, "` must be `TRUE` or `FALSE`"))
  }
}

test$interval_arg <- function(arg, lower, upper, alternative = NULL) {
  arg_name <- rlang::ensym(arg)
  if (!identical(arg, alternative) && (!is.numeric(arg) || (arg <= lower || upper <= arg))) {
    stop(paste0(
      "`", arg_name, "` must be ", alternative,
      " or ", lower, " < x < ", upper
    ))
  }
}

test$data <- function(arg, env) {
  arg_name <- rlang::ensym(arg)

  if (! class(arg) %in% get_available_methods(as.data.frame)) {
    cli::cli_abort("{arg_name} is not coercible to data.frame", call = env)
  }
}

test$ggplot_arg <- function(args) {
  arg_name <- deparse(rlang::ensym(arg))
  arg_subname <- gsub("args_(.+)", "\\1", arg_name)
  if (arg_subname == "facet") {
    cond <- all(names(args) %in% union(
      names(formals(ggplot2::facet_grid)),
      names(formals(ggplot2::facet_wrap))
    ))
    if (!cond) warning(paste0("Unknown arguments in ", arg_name))
  } else {
    cond <- all(names(args) %in% names(
      formals(match.fun(paste0("ggplot2::geom_", arg_subname)))
    ))
    if (!cond) warning(paste0("Unknown arguments in ", arg_name))
  }
}

test$type <- function(arg, types, env) {
  arg_name <- rlang::ensym(arg)

  notpass <- tolower(types) %>%
    purrr::map_lgl(~get(glue("is_{.x}"))(arg)) %>%
    any() %>%
    `!`()

  if (notpass) {
    cli::cli_abort("{.var {arg_name}} is not one of {pluralize_or(types)}",
      call = env
    )
  }
}


setup <- list()

setup$ignore_cols <- function(arg) {
  arg_name <- rlang::ensym(arg)
  isnumeric_cols <- sapply(arg, \(x) is_integer(x) | is_double(x))

  if (all(isnumeric_cols)) {
    arg
  } else {
    cli::warn("Ignoring non numeric columns in `{arg_name}`")
    arg[, isnumeric_cols]
  }
}


get_names <- function(x, type = NULL) {
  if (inherits(x, c("data.frame", "matrix"))) {
    return(colnames(x))
  }
  if (inherits(x, "varest")) {
    return(names(x$varresult))
  }
  if (inherits(x, "varprd")) {
    return(names(x$fcst))
  }
  if (inherits(x, "varstabil")) {
    return(x$names)
  }
  if (inherits(x, "varfevd")) {
    return(names(x))
  }
  if (inherits(x, "varirf")) {
    return(x[[type]])
  }
}
