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
#' strings <- simplewords(strings)
#'
#' clean_name <- simplewords("Latin-multi^name with strange/characters")
#'
#' df$Name <- simplewords(df$Name, case = "lower")
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
