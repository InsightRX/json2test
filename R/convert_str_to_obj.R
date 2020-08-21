#' Internal function used for testing
#'
#' @param x string
convert_str_to_obj <- function(x) {
  x <- irxtools::str_replace(x, "Rscript api.R ", "")
  s <- irxtools::str_split(x, " ")[[1]]
  s2 <- irxtools::str_split(s, "=")
  obj <- list()
  for(i in seq(s2)) {
    obj[[s2[[i]][1]]] <- s2[[i]][2]
  }
  obj
}
