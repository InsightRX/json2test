#' Print test result collector
#'
#' @export
print_test_report <- function() {
  if(!is.null(test_result_collector)) {
    res <- data.frame(test_result_collector)
    colnames(res) <- c("Function", "Test", "Result")
    success <- sum(res$Result == "OK")/length(res[,1])*100
    cat(paste0("Overall passed: ", round(success, 1), "%\n\n"))
    tab <- do.call(data.frame, aggregate(Result ~ Function, res,
                     FUN = function(x) c(Passed = sum(as.character(x) == "OK"),
                                         Failed = sum(as.character(x) != "OK"),
                                         Total = length(x)), drop = FALSE))
    colnames(tab) <- gsub("Result\\.", "", colnames(tab))
    tab$Percent <- 100 * round(tab$Passed / tab$Total, 3)
    if(max(tab$Failed) > 0) {
       cat(paste0("\n------------ Failed tests (",sum(res$Result != "OK"),"/",length(res[,1]),") ------------------\n"))
       print(res[res$Result != "OK", c("Function", "Test")])
    }
    cat("\n-------------- Test report ------------------------\n")
    print(data.frame(tab[order(tab$Percent, decreasing = TRUE),]))
    cat("----------------------------------------------------\n")
    if(success < 100) {
      stop("Not all tests were successful.")
    }
  } else {
    stop("Sorry, no test results available.")
  }
}

#' Reset test result collector
#'
#' @export
reset_test_report <- function() {
  test_result_collector <<- c()
}
