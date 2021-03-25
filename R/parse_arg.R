#' Parse string, assuming JSON format
#' If arguments are not valid JSON, returns stop. Adapted from opencpu.
#' @param x object
#' @export
parse_arg <- function(x){

  #special for json obj
  if(inherits(x, "AsIs")){
    class(x) <- utils::tail(class(x), -1);
    return(x);
  }

  #cast (e.g. for NULL)
  x <- as.character(x)

  #if string starts with { or [ we test for json
  if(grepl("^[ \t\r\n]*(\\{|\\[)", x)) {
    if(jsonlite::validate(x)) {
      return(jsonlite::fromJSON(x));
    }
  }

  #failed to parse argument
  stop("Invalid argument: ", x, "\n.Cannot be parsed to json.");
}
