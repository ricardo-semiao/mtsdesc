test <- list()

test$type <- function(arg, types, env) {
  arg_name <- rlang::ensym(arg)

  notpass <- tolower(types) %>%
    purrr::map_lgl(~ do.call(glue("is_{.x}"), list(arg))) %>%
    any() %>%
    `!`()

  if (notpass) {
    cli_abort(
      "{.var {arg_name}} is not one of {.or {types}}",
      call = env
    )
  }
}

test$index <- function(index, n, required = NULL, env) {
  arg_name <- rlang::ensym(arg)
  required <- required %||% deparse(rlang::enexpr(n))

  if (anyDuplicated(index)) {
    cli_abort(
      "{.var index} musn't have duplicated entries.",
      call = env
    )
  }
  if (length(index) != n) {
    cli_abort(
      "{.var index} must have a length of {required}.",
      call = env
    )
  }
}

test$category <- function(arg, options, env) {
  arg_name <- rlang::ensym(arg)

  if (!all(arg %in% options)) {
    cli_abort(
      "{.var {arg_name}} must contain only {.or {options}}",
      call = env
    )
  }
}

test$interval <- function(arg, lower, upper, alt = ".no_alt", env) {
  arg_name <- ensym(arg)
  text_alt <- ""

  notpass <- !(is_integer(arg) || is_double(arg)) ||
    (arg < lower || upper < arg)

  if (!identical(alt, ".no_alt")) {
    notpass <- !identical(arg, alt) && notpass
    text_alt <- glue("equal to `{alt}`, or ")
  }

  if (notpass) {
    cli_abort(
      "
      {.var {arg_name}} must be {text_alt}numeric and '{lower} < {arg_name} \\
      < {upper}'.
      ",
      call = env
    )
  }
}
