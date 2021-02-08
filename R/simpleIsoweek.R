#' Title Get Iso year-week from a date object
#'
#' @param x Date value or vector to convert to Iso year-week
#' @param integer Return an integer instead of a cahracter vector. Default is FALSE
#'
#' @return Returns Iso year-weeks as strings in the format YYYYWW
#'
#' @examples
#' df$Isoweek <- simpleIsoweek(df$sample_date)
#' 
#' @export
simpleIsoweek <- function(x, integer){
  yearweek <- paste0(lubridate::isoyear(x), stringr::str_pad(lubridate::isoweek(x), 2, pad = "0"))
  if(missing("integer")) integer <- FALSE
  if(integer == TRUE){
    yearweek <- as.integer(yearweek)
  }
  return(yearweek)
}



#' Title Get Iso year-week from a date object
#'
#' @param x Date value or vector to convert to Iso year-week
#'
#' @return Returns Iso year-weeks as strings in the format YYYYWW
#'
#' @examples
#' df$Isoweek <- simpleIsoweek(df$sample_date)
#' 
#' @export
simpleIsoweek <- function(x, complete){
  yearweek <- paste0(lubridate::isoyear(x), stringr::str_pad(lubridate::isoweek(x), 2, pad = "0"))
  return(yearweek)
}