#' Safely parse JSON to R object
#'
#' @param obj object read from JSON
#' @param parse_functions list of function names, keys are the elements in the object
parse_json_test <- function(obj, parse_functions = list()) {
  for(key in names(parse_functions)) {
    if(!is.null(obj[[key]]) && length(obj[[key]]) > 0) {
      suppressWarnings( {
        tmp <- list()
        tmp[[key]] <- obj[[key]]
        obj[[key]] <- do.call(parse_functions[[key]], tmp)
      })
    }
  }
  return(obj)
}
