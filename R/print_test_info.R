#' Print some info on number of tests per function
#'
#' @param package package name
#' @export
print_test_info <- function(package = NULL) {
  if(!is.null(package)) {
    tests <- dir(system.file("/test", package=package))
    tab <- c()
    for(key in tests) {
      tmp <- rjson::fromJSON(file = system.file(paste0("test/", key), package = "insightrxr"))
      module <- stringr::str_replace_all(key, "(.json)", "")
      tab <- rbind(tab, cbind(module, length(names(tmp))))
    }
    tab <- data.frame(tab)
    colnames(tab) <- c("Module", "Tests")
    print(tab)
  } else {
    message("No package specified, can't print test info.")
  }
}
