#' Title Remove letters and special characters from a string to return a numeric value
#'
#' @param x string or vector with numeric values for cleaning
#' @param fill Value to fill empty values with. Default NULL will not fill empty values
#'
#' @return number or vector of numbers
#'
#' @examples
#' # x <- c("dfju", "LASK3", "dfpisdu092", "9376", 3098)
#' # x <- simplenumber(x, fill = -1)
#'
#' @export
simplenumber <- function(x, fill = NULL){
  x <- gsub("[^0-9]", "", x)
  x <- as.numeric(x)
  if(!is.null(fill)){
    x[is.na(x)] <- fill
  }
  return(x)
}


#' Remove special characters and numbers and format to UTF-8 encoding with standardised capitalisation
#'
#' Takes a vector with character elements, removes special characters and numbers and formats them to UTF-8 encoding to make these consistent and compatible with most R analysis procedures. Returns strings with the first letter capitalised and the rest in lower case
#'
#' @param x string or character vector
#'
#' @param case Optional; case of words to return; upper, lower or title (first letter upper case and the rest lower case).
#'   Default is title
#' @param encode Optional; Logical; attempt to encode as UTF-8. Default is TRUE.
#'
#' @return Returns the input in UTF-8 format with special characters replaced with spaces and numbers removed.
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords encoding, special characters
#'
#' @examples
#' # strings <- simplewords(strings)
#' # clean_name <- simplewords("Latin-multi^name with strange/characters")
#' # df$Name <- simplewords(df$Name, case = "lower")
#'
#'
#' @export
simplewords <- function(x, case, encode = TRUE){
  if(encode){
    x <- stringi::stri_encode(x, "", "UTF-8")
    x <- stringi::stri_trans_general(x, "name-any")
    x <- iconv(x, to = "UTF-8")
  }
  x <- gsub("[[:digit:]]+", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  if (missing(case)) case <- "title"
  if (case == "upper"){
    x <- toupper(x)
  } else if(case == "lower"){
    x <- tolower(x)
  } else if (case == "title"){
    x <- stringr::str_to_title(x)
  } else {
    stop("font case not recognised")
  }
  x <- trimws(x)
  x <- gsub("  "," ", x)
  return(x)
}


#' Format dates
#'
#' Function will leave POSIX dates and attempt to convert dates
#' which are in string format, excel exported numeric format or
#' numeric format into the "best guess" date. Where vector provided
#' with dates in more than one format will attempt to convert them all.
#'
#' @param x dates to format. Takes a single date, vectors or a column of data.
#' @param char Optional value to return for character strings;
#' Default is NA. Use historic date values to manage categories
#'  where date not known but label required for downstream processing.
#' @param silent Optional suppress warning of occurrences which failed to parse.
#' Default is TRUE.
#'
#' @return Returns input formatted as Date
#' @import data.table
#'
#' @examples
#' # Format a single string into a date
#' # date <- simpledates("2020739")
#'
#' # Format a data.frame column into a consistent date format
#' # df$column <- simpledates(df$column)
#'
#' # Format all column from within a data.table where the columns
#' # contain the string date into date format
#' # for (col in (names(dt)[grepl("date", names(dt))])){
#' #   dt[,(col) := simpledates(dt[,get(col)])]
#' #  }
#'@export
simpledates <- function(x, char = NA, silent = TRUE){
  if(any(class(x) %in% c("POSIXct", "POSIXt"))) {
    suppressWarnings(y <- try(as.Date(x)))
  } else {
    suppressWarnings(y <- try(as.Date(lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd")))))
  }
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


#' Format a date to isoyear-isoweek
#'
#' @param x Date(s) to be converted to isoyear-isoweek
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


#' Title Create a year-week sequence
#'
#' @param from Start date in yearweek (YYYYWW) or date format
#' @param to End data in yearweek (YYYYWW) or date format
#' @param by Increments for sequence in weeks
#' @param rangef Format of time-range (from and to); one of yearweek or date.Default is yearweek.
#'
#' @return Returns a sequence of yearweek values for the range
#'
#' @examples
#' # seq.yearweek(from = 202102, to = 202105, by = 1, rangef = "yearweek")
#' # seq.yearweek(from = "20210205", to = "20210510", by = 2, rangef = "date")
#'
#'
#' @export
seq.yearweek <- function(from, to, by, rangef = "yearweek"){
  if(!(rangef %in% c("date", "yearweek"))) stop("rangef not recognised. Must be 'yearweek' or 'date'")
  # Dates
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
  # Convert days to weeks
  by <- by *7
  dates <- seq.Date(from = from_out, to = to_out, by = by)
  yearweek <- yearweek(dates)
  return(yearweek)
}

