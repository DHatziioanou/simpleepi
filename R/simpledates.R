#' Format dates
#'
#'
#' Format dates either from a string format or an excel exported numeric format
#'
#' @param x dates to format. Takes a single date, vectors or a column of data.
#'
#' @return
#' @export
#'
#' @examples
#' # Format a single string into a date
#' date <- fix_dates("2020739")
#'
#' # Format a data.frame column into a consitent date format
#' df$column <- fix_dates(df$column)
#'
#' # Format all column from within a data.table where the columns contain the string date into date format
#' for (col in (names(dt)[grepl("date", names(dt))])){dt[,(col) := simpledates(dt[,get(col)])]}
#'
simple_dates <- function(x){
 if(suppressWarnings(sum(!(is.na(lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd")))))) == 0) {
      x <- as.numeric(x)
      x <- as.Date(x, origin = "1899-12-30")
     }
 x <- as.Date(lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd")))
 return(x)
}
