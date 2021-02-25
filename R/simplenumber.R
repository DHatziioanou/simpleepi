

#' Title
#' Remove letters and special characters from a string to return a numeric value
#' @param x string or vector with numeric values for cleaning
#'
#' @return number or vector of numbers
#'
#' @examples
#' x <- c("dfju", "LASK3", "dfpisdu092", "9376", 3098)
#' x <- number_clean(x, fillempty = T, fillvalue = -1)
#'
#' @export
simplenumber <- function(x, fillempty, fillvalue){
  x <- gsub("[^0-9.-]", "", x)
  x <- as.numeric(x)
  if(missing("fillempty")) fillempty <- FALSE

  if(isTRUE(fillempty)){
    if(missing("fillvalue")) fillvalue <- 0
    x[is.na(x)] <- fillvalue
  }
  return(x)
}
