#' Print test result collector
#'
#' @export
print_test_report <- function() {
  if(!is.null(test_result_collector)) {
    cat("\n-------------- Test report ------------------------\n")
    res <- data.frame(test_result_collector)
    colnames(res) <- c("Function", "Test", "Result")
    success <- sum(res$Result == "OK")/length(res[,1])*100
    cat(paste0("Overall passed: ", round(success, 1), "%\n\n"))
    passed  <- function(x) { return(sum(as.character(x) == "OK")) }
    failed  <- function(x) { return(sum(as.character(x) != "OK")) }
    tab <- res %>% dplyr::group_by(Function) %>% dplyr::summarise("Passed" = passed(Result),
                                                    "Failed" = failed(Result),
                                                    "Total" = length(Result),
                                                    "Percent" = round(100*passed(Result)/length(Result),1))
    print(data.frame(tab))
    if(sum(res$Result != "OK") > 0) {
      cat(paste0("\n------------ Failed tests (",sum(res$Result != "OK"),"/",length(res[,1]),") ------------------\n"))
      if(sum(res$Result != "OK") > 0) {
        print(res[res$Result != "OK",])
      }
      cat("----------------------------------------------------\n")
      if(success < 100) {
        stop("Not all tests were successful.")
      }
    }
  } else {
    stop("Sorry, no test results available.")
  }
}

`%>%` <- dplyr::`%>%`

#' Reset test result collector
#'
#' @export
reset_test_report <- function() {
  test_result_collector <<- c()
}
