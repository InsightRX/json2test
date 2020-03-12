#' Get nested value from a list
#'
#' @param l list
#' @param address address in list, nesting separated using . (similar to $ in R)
get_nested_value <- function(l = list(), address = "") {
  suppressWarnings(
    if(!is.null(address) && length(address) > 0) {
      if(stringr::str_detect(address, "\\.")) {
        vec <- unlist(stringr::str_split(address, "\\."))
        if(length(vec) == 2 && vec[2] %in% c("length", "nrow", "ncol")) {
          return(do.call(vec[2], list(l[[vec[1]]])))
        } else {
          if(!is.na(as.numeric(vec[1]))) {
            if(class(l) == "list") {
              return(get_nested_value(l[[as.numeric(vec[1])]], stringr::str_c(vec[-1], collapse = ".")))
            } else {
              return(get_nested_value(l[as.numeric(vec[1])], stringr::str_c(vec[-1], collapse = ".")))
            }
          } else {
            return(get_nested_value(l[[vec[1]]], stringr::str_c(vec[-1], collapse = ".")))
          }
        }
      } else {
        if(!is.na(as.numeric(address))) { # then treat l as vector, not list!
          return(l[as.numeric(address)])
        } else {
          return(l[[address]])
        }
      }
    } else {
      return(l)
    }
  )
}
