#' Convert to numeric from factors and character
#'
#' @param x factor or character
as.num <- function(x) {
  return(as.numeric(as.character(x)))
}
