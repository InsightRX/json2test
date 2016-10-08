#' Print test result collector
#'
#' @export
print_test_report <- function(failed_only = TRUE) {
  if(!is.null(test_result_collector)) {
    cat("\n-------------- Test report -----------------------\n")
    # res <- (t(data.frame(test_result_collector)))
    res <- data.frame(test_result_collector)
    colnames(res) <- c("Function", "Test", "Result")
    if(failed_only) {
      cat(paste0("Faild tests: (",sum(res$Result != "OK"),"/",length(res[,1]),")\n"))
      if(sum(res$Result != "OK") > 0) {
        print(res[,res$Result != "OK"])
      }
    } else {
      print(res)
    }
    cat("---------------------------------------------------\n")
    success <- sum(res$Result == "OK")/length(res[,1])*100
    cat(paste0("Success: ", round(success, 1), "%"))
    cat("\n---------------------------------------------------\n")
    if(success < 100) {
      stop("Not all tests were successful.")
    }
  } else {
    stop("Sorry, no test results available.")
  }
}
