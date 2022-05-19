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
   suppressWarnings(y[which(!is.na(x))[which(is.na(y))]] <- as.Date(as.numeric(x[which(!is.na(x))[which(is.na(y))]]), origin = "1899-12-30"))
   # Label character and other failed formats
   suppressWarnings(if(!is.na(char)) y[which(!is.na(x))[which(is.na(y))]] <- try(as.Date(lubridate::parse_date_time(char, orders = c("dmy", "mdy", "ymd")))))
   # Failed to parse;
   if(!silent) warning(paste(length(x[which(!is.na(x))[which(is.na(y))]]),"failed to parse;", paste(
      ifelse(length(x[which(!is.na(x))[which(is.na(y))]])>10,
             x[which(!is.na(x))[which(is.na(y))]][1:10],
             x[which(!is.na(x))[which(is.na(y))]]),
      collapse = ", ")))
   return(y)
}




