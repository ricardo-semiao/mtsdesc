test <- list()

test$type <- function(arg, types, n = NULL, env) {
  arg_name <- rlang::ensym(arg)

  test_args <- if (is_null(n)) list(x = arg) else list(x = arg, n = n)

  notpass <- tolower(types) %>%
    purrr::map_lgl(~ do.call(glue("is_{.x}"), test_args)) %>%
    any() %>%
    `!`()

  text_n <- if (!is_null(n)) {
    ", and its length must be {.val n}"
  } else {
    ""
  }

  if (notpass) {
    cli_abort(
      "The type of {.var {arg_name}} must be{? one of} {.or {types}}{text_n}.",
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

  if (!identical(arg, alt)) test$type(arg, c("integer", "double"), 1, env = env)

  notpass <- arg < lower || upper < arg

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

test$args <- function(..., env) {
  args_names <- ensyms(...)
  args <- list(...)

  for (i in seq_along(args)) {
    if (!is_list(args[[i]])) {
      cli_abort(
        "{.var {args_names[[i]]}} should be a list.",
        call = env
      )
    }
  }
}

test$unused <- function(arg, x, class, env) {
  arg_name <- ensym(arg)

  if (class(x) == class) {
    cli_inform(
      "{.val {arg_name}} is unused if `x` is of class {class}.",
      call = env
    )
  }
}
