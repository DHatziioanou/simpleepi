#' Title Format a date to isoyear-isoweek
#'
#' @param x Date
#'
#' @return Returns x in format YYYYWW where WW is the isoweek and YYYY is the isoyear.
#'
#' @examples
#' # yw <- simpleyearweek(date)
#' # x$yw <- simpleyearweek(x$date)
#'
#' @export
yearweek  <- function(x){
  if(any(!(class(x) %in% c("Date", "IDate")))) x <- simpledates(x)
  week <- lubridate::isoweek(x)
  year <- lubridate::isoyear(x)
  yearweek <- suppressWarnings(as.integer(paste0(year,stringr::str_pad(string = week, width = 2, side = "left", pad = 0))))
  return(yearweek)
}
