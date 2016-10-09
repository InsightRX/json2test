#' Print test result collector
#'
#' @export
print_test_report <- function() {
  if(!is.null(test_result_collector)) {
    cat("\n-------------- Test report ------------------------\n")
    # res <- (t(data.frame(test_result_collector)))
    res <- data.frame(test_result_collector)
    colnames(res) <- c("Function", "Test", "Result")
    success <- sum(res$Result == "OK")/length(res[,1])*100
    cat(paste0("Overall passed: ", round(success, 1), "%\n\n"))
    pct <- function(x) { return(paste0(round(100*sum(as.character(x) == "OK")/length(x),1), "%")) }
    pass <- aggregate(as.character(res$Result), by = list(Function = res$Function), FUN = pct)
    colnames(pass) <- c("Function", "Pass %")
    print(pass)
    cat(paste0("\n------------ Failed tests (",sum(res$Result != "OK"),"/",length(res[,1]),") ------------------\n"))
    if(sum(res$Result != "OK") > 0) {
      print(res[res$Result != "OK",])
    }
    cat("----------------------------------------------------\n")
    if(success < 100) {
      stop("Not all tests were successful.")
    }
  } else {
    stop("Sorry, no test results available.")
  }
}
