#' Safely parse JSON to R object
#'
#' @param obj object
parse_json_test <- function(obj) {
  if(!is.null(obj$tdm) && length(obj$tdm) > 0) {
    suppressWarnings( {
      obj$tdm <- convert_tdm_json_object(obj$tdm)
    })
  }
  return(obj)
}
