# Collect messages with a calling handler rather than capture.output(), since
# test runners (e.g. testit::test_pkg) may muffle messages before they reach
# a message sink.
capture_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  msgs
}

testit::assert("Compares test and ref json for function supplied as function: pass", {
  test_function <- function(x){ list(names(x)) }
  out <- capture_messages(json_test(func = test_function,
                                    package = "json2test",
                                    test_id = "test_cases",
                                    list_as_args = TRUE,
                                    fail_if_not_exists = FALSE))
  (any(grepl("\u2713", out)))
})

testit::assert("Compares test and ref json for function supplied as function: fail", {
  test_function <- function(x){ list(lapply(x, is.numeric)) }
  out <- capture_messages(json_test(func = test_function,
                                    package = "json2test",
                                    test_id = "test_cases",
                                    list_as_args = TRUE,
                                    fail_if_not_exists = FALSE))
  (!any(grepl("\u2713", out)))
  (any(grepl("\\[ \\]", out)))
})



testit::assert("Checks R-specific values and data types: pass", {
  test_function <- function(x){
    list(
      is_null = NULL,
      is_na = NA,
      is_nan = NaN,
      is_df = data.frame(a = 1, b = 2),
      is_int = 1L,
      is_num = 1.5,
      is_chr = "a"
    )
  }
  out <- capture_messages(json_test(func = test_function,
                                    package = "json2test",
                                    test_id = "special_values",
                                    fail_if_not_exists = FALSE))
  (sum(grepl("\u2713", out)) == 8 && !any(grepl("\\[ \\]", out)))
})

testit::assert("Checks R-specific values and data types: fail", {
  test_function <- function(x){
    list(
      is_null = 1,
      is_na = NaN,
      is_nan = NA,
      is_df = list(a = 1, b = 2),
      is_int = 1,
      is_num = "1",
      is_chr = NA
    )
  }
  out <- capture_messages(json_test(func = test_function,
                                    package = "json2test",
                                    test_id = "special_values",
                                    fail_if_not_exists = FALSE))
  (sum(grepl("\\[ \\]", out)) == 8)
})
