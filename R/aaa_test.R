test_combinations <- function(
    fun_name, args, combs_name,
    expect = "expect_doppelganger", evaluate = TRUE) {
  k <- prod(sapply(args, length))

  args_names <- paste0(
    c("", names(args)[-1]),
    c("", rep(" = ", length(args) - 1))
  )
  args_combs <- lapply(args, \(x) rep(x, k / length(x)))

  args_combs_trans <- list()
  for (i in seq_along(args_combs)) {
    for (j in seq_along(args_combs[[i]])) {
      nm <- names(args_combs[[i]])[[j]]
      args_combs_trans[[paste0("test", j)]][[nm]] <- args_combs[[i]][[j]]
    }
  }

  exprs <- purrr::imap(args_combs_trans, function(x, test_number) {
    test_name <- paste(names(x), collapse = "-")
    fun_call <- paste0(fun_name, "(", paste0(args_names, x, collapse = ", "), ")")
    quoted_test <- paste0(
      "test_that('", combs_name, " - ", test_number, ": ", test_name, "', {",
      expect, "(",
      "'", test_name, "', ",
      fun_call,
      ")",
      "})"
    ) %>% parse(text = .)
    if (evaluate) eval(quoted_test) else parse(text = fun_call)
  })

  invisible(exprs)
}
