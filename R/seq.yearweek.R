#' Title Create a year-week sequence
#'
#' @param from Start date in yearweek (YYYYWW) or date format
#' @param to End data in yearweek (YYYYWW) or date format
#' @param by Increments for sequence in weeks
#' @param rangef Format of from and to; one of yearweek or date.
#'
#' @return Returns a sequence of yearweek values for the range
#'
#' @examples
#' seq.yearweek(from = 202102, to = 202105, by = 1, rangef = "yearweek")
#' seq.yearweek(from = "20210205", to = "20210510", by = 2, rangef = "date")
#'
#'
#' @export
seq.yearweek <- function(from, to, by, rangef = "yearweek"){
  if(!(rangef %in% c("date", "yearweek"))) stop("rangef not recognised. Must be 'yearweek' or 'date'")
  if(rangef == "date"){
    from_out <- simpledates(from)
    to_out <- simpledates(to)
  }
  if(rangef == "yearweek"){
    from_out <- try(as.Date(paste0(substr(from,1,4), ":",as.integer(substr(from,5,6)), ":1"), format = "%Y:%W:%u")-7)
    if(class(from_out) =="try-error"){
      from_out <- try(as.Date(paste0(substr(to,1,4), ":",as.integer(substr(to,5,6)), ":7"), format = "%Y:%W:%u"), silent = TRUE)
    }
    to_out <- try(as.Date(paste0(substr(to,1,4), ":",as.integer(substr(to,5,6)), ":7"), format = "%Y:%W:%u"), silent = TRUE)
    if(class(to_out) =="try-error"){
      to_out <- try(as.Date(paste0(substr(to,1,4), ":",as.integer(substr(to,5,6)), ":1"), format = "%Y:%W:%u"), silent = TRUE)
    }
  }
  by <- by *7
  dates <- seq.Date(from = from_out, to = to_out, by = by)
  yearweek <- yearweek(dates)
  return(yearweek)
}

