#' Check whether a reference value is a special keyword
#'
#' Special keywords are character strings starting with \code{$}, used in reference
#' JSON to check for R-specific values and data types that cannot be
#' represented in JSON directly.
#'
#' @param ref reference value
is_special_reference <- function(ref) {
  is.character(ref) && length(ref) == 1 && !is.na(ref) &&
    grepl("^\\$(exists|null|not_null|na|nan|class:.+|type:.+)$", ref)
}

#' Check a calculated value against a special reference keyword
#'
#' Supported keywords:
#' \itemize{
#'   \item \code{$exists}: value is not NULL, has length > 0, and is not NA
#'   \item \code{$null}: value is NULL
#'   \item \code{$not_null}: value is not NULL
#'   \item \code{$na}: value is NA (all elements NA, but not NaN)
#'   \item \code{$nan}: value is NaN (all elements NaN)
#'   \item \code{$class:<class>}: value inherits from class \code{<class>}, e.g.
#'     \code{$class:data.frame}. Multiple classes can be separated by \code{|},
#'     in which case any may match.
#'   \item \code{$type:<type>}: \code{typeof()} of value equals \code{<type>},
#'     e.g. \code{$type:double}
#' }
#'
#' @param ref reference keyword
#' @param calc calculated value
#' @return logical of length 1
check_special_value <- function(ref, calc) {
  if(ref == "$exists") {
    return(!is.null(calc) && length(calc) > 0 && !all(is_na_atomic(calc)))
  }
  if(ref == "$null") {
    return(is.null(calc))
  }
  if(ref == "$not_null") {
    return(!is.null(calc))
  }
  if(ref == "$na") {
    return(is.atomic(calc) && length(calc) > 0 &&
           all(is.na(calc)) && !any(is_nan_atomic(calc)))
  }
  if(ref == "$nan") {
    return(length(calc) > 0 && all(is_nan_atomic(calc)))
  }
  if(grepl("^\\$class:", ref)) {
    classes <- strsplit(sub("^\\$class:", "", ref), "\\|")[[1]]
    return(inherits(calc, classes))
  }
  if(grepl("^\\$type:", ref)) {
    return(typeof(calc) == sub("^\\$type:", "", ref))
  }
  stop("Unknown special reference value: ", ref)
}

is_na_atomic <- function(x) {
  if(is.atomic(x)) is.na(x) else rep(FALSE, length(x))
}

is_nan_atomic <- function(x) {
  if(is.numeric(x) || is.complex(x)) is.nan(x) else rep(FALSE, length(x))
}

#' Format a calculated value for printing in test output
#'
#' @param x value
format_value <- function(x) {
  if(is.null(x)) return("NULL")
  if(is.atomic(x) && length(x) <= 10) return(paste(x, collapse = ", "))
  paste0("<", paste(class(x), collapse = "/"), ">")
}
