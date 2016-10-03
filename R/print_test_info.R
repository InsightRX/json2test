#' Print some info on number of tests per function
#'
#' @export
print_test_info <- function() {
  tests <- dir(system.file("/test", package="insightrxr"))
  tab <- c()
  for(key in tests) {
    tmp <- rjson::fromJSON(file = system.file(paste0("test/", key), package = "insightrxr"))
    module <- stringr::str_replace_all(key, "(advice_|.json)", "")
    tab <- rbind(tab, cbind(module, length(names(tmp))))
  }
  tab <- data.frame(tab)
  colnames(tab) <- c("Module", "Tests")
  print(tab)
}
