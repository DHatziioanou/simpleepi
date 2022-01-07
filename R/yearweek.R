#' Title Format a date to year-isoweek
#'
#' @param x Date
#'
#' @return Returns x in format YYYYWW
#'
#' @examples
#' yw <- simpleyearweek(date)
#' x$yw <- simpleyearweek(x$date)
#'
#' @export
yearweek  <- function(x){
  if(any(!(class(x) %in% c("Date", "IDate")))) x <- simpledates(x)
  week <- data.table::isoweek(x)
  year <- data.table::year(x)
  yearweek <- suppressWarnings(as.integer(paste0(year,stringr::str_pad(string = week, width = 2, side = "left", pad = 0))))
  return(yearweek)
}
