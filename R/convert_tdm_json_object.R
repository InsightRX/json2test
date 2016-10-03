#' Safely convert the tdm object in JSON
#'
#' @param tdm tdm object
convert_tdm_json_object <- function(tdm) {
  ## the JSON table is converted to a list in R, so need to convert back.
  ## this code is a bit ad hoc but it should work fine.
  new_tdm <- c()
  nam <- names(tdm[[1]])
  for(i in 1:length(tdm)) {
    vec <- c()
    for (j in 1:length(nam)) {
      if(!is.null(tdm[[i]][[nam[j]]])) {
        vec <- c(vec, tdm[[i]][[nam[j]]])
      } else {
        vec <- c(vec, 0)
      }
    }
    new_tdm <- rbind(new_tdm, vec)
  }
  new_tdm <- data.frame(new_tdm)
  names(new_tdm) <- nam
  strings <- c("reference", "dose_id", "timesec", "type", "subtype", "unit", "_id", "patient_id", "firstdose", "abs_time")
  for (i in 1:length(nam)) {
    new_tdm[,i] <- as.character(new_tdm[,i])
    new_tdm[is.na(new_tdm[,i]),] <- "0"
    if(!nam[i] %in% strings) {
      new_tdm[,i] <- as.numeric(new_tdm[,i])
    }
  }
  return(new_tdm)
}
