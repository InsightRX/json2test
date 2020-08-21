#' Run all JSON tests included with package
#'
#' @param func function to test, also refers to name of JSON file in test/reference folders
#' @param package name of package
#' @param skip_func skip this functions (vector of character)
#' @param ... arguments to pass to `json_test` function
#' @export

json_test_all <- function(
  func = NULL,
  package = NULL,
  skip_func = c(),
  ...) {
  if(is.null(package)) {
    stop("please specify package to be tested")
  }
  tests <- dir(system.file("/test", package = package))
  if(!is.null(skip_func)) {
    tests <- tests[!tests %in% skip_func]
  }
  for(key in tests) {
    key <- irxtools::str_replace(key, ".json", "")
    print(key)
    json_test(func = key, package = package, ...)
  }
}
