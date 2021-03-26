testit::assert("Compares test and ref json for function supplied as function: pass", {
  test_function <- function(x){ list(names(x)) }
  out <- capture.output(json_test(func = test_function,
                                  package = "json2test",
                                  test_id = "test_cases",
                                  list_as_args = TRUE,
                                  fail_if_not_exists = FALSE),
                        type = "message")
  any(grepl("✓", out))
})

testit::assert("Compares test and ref json for function supplied as function: fail", {
  test_function <- function(x){ list(lapply(x, is.numeric)) }
  out <- capture.output(json_test(func = test_function,
                                  package = "json2test",
                                  test_id = "test_cases",
                                  list_as_args = TRUE,
                                  fail_if_not_exists = FALSE),
                        type = "message")
  !any(grepl("✓", out))
})


