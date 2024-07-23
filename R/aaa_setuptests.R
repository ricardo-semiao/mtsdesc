get_available_methods <- function(f) {
  f_name <- as_string(ensym(f))
  utils::methods(f_name) %>%
    attr("info") %>%
    rownames() %>%
    gsub(paste0(f_name, "."), "", ., fixed = TRUE)
}


test <- list()

test$type <- function(arg, types, env) {#, n = NULL
  arg_name <- rlang::ensym(arg)

  notpass <- tolower(types) %>%
    purrr::map_lgl(~get(glue("is_{.x}"))(arg)) %>% #, n
    any() %>%
    `!`()

  if (notpass) {
    cli::cli_abort("{.var {arg_name}} is not one of {.or {types}}",
      call = env
    )
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

test$category <- function(arg, options, env) {
  arg_name <- rlang::ensym(arg)

  if (!arg %in% options) {
    cli::cli_abort("{.var {arg_name}} must be {.or {options}}", call = env)
  }
}

test$interval <- function(arg, lower, upper, alt = NULL, env) {
  arg_name <- ensym(arg)
  text_add <- ""

  notpass <- !(is_integer(arg) || is_double(arg)) ||
    (arg < lower || upper < arg)
  if (!is_null(alt)) {
    notpass <- !identical(arg, alt) && notpass
    text_add <- glue(", or equal `{alt}`")
  }

  if (notpass) {
    cli::cli_abort("
    {.var {arg_name}} must be '{lower} < {arg_name} < {upper}'{text_add}.
    ",
      call = env
    )
  }
}
