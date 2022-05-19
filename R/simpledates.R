#' Format dates
#'
#'
#' Format dates either from a string format or an excel exported numeric format
#'
#' @param x dates to format. Takes a single date, vectors or a column of data.
#' @param char valut to return for character strings. Use 00000002 to return lable value of "1900-01-01"
#'
#' @return
#' @import data.table
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
#'@export
simpledates <- function(x, char = NA, silent = TRUE){
   suppressWarnings(y <- try(as.Date(lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd")))))
   # Manage excel formats
   f <- which(!is.na(x))[which(!is.na(x)) %in% which(is.na(y))]
   if(length(f) !=0) suppressWarnings(y[f] <- as.Date(as.numeric(x[f]), origin = "1899-12-30"))
   # Label character and other failed formats
   f <- which(!is.na(x))[which(!is.na(x)) %in% which(is.na(y))]
   if(length(f) !=0) suppressWarnings(if(!is.na(char)) y[f] <- try(as.Date(lubridate::parse_date_time(char, orders = c("dmy", "mdy", "ymd")))))
   # Failed to parse;
   if(!silent) {
      f <- which(!is.na(x))[which(!is.na(x)) %in% which(is.na(y))]
      warning(paste(length(f),"failed to parse;", ifelse(length(f)>10, paste(x[f][1:10], collapse = ", ", sep = ",") ,paste(x[f], collapse = ", ", sep = ","))))
   }
   return(y)
}




