#' Remove special characters and numbers and format to UTF-8 encoding with standardised capitalisation
#'
#' Takes a vector with character elements, removes special characters and numbers and formats them to UTF-8 encoding to make these consistent and compatible with most R analysis procedures. Returns strings with the first letter capitalised and the rest in lower case
#'
#' @param x string or character vector
#'
#' @param case Case of words to return; one of upper, lower or title for the first letter uppercase and the rest lower case
#'
#' @return Returns the input in UTF-8 format with special characters replaced with spaces and numbers removed.
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords encoding, special characters
#'
#' @examples
#' strings <- tidywords(strings)
#'
#' clean_name <- tidywords("Latin-multi^name with strange/characters")
#'
#' df$Name <- tidywords(df$Name, case = "lower")
#'
#'
#' @export
simplewords <- function(x, case){
  x <- stringi::stri_encode(x, "", "UTF-8")
  x <- stringi::stri_trans_general(x, "name-any")
  x <- iconv(x, to = "UTF-8")
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

  return(x)
}
