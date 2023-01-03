#' Title Change character encoding to UTF-8 with optional case management
#'
#' @param x  Character object or vector to encode.
#' @param case Optional; Default is none (no formatting). Other options are upper, lower or title.
#'
#' @return returns the input encoded to UTF-8
#'
#' @examples
#' x <- "troublesome character string"
#' x <- simpleencode(x)
#'
#' @export
simpleencode <- function(x, case = "none"){
  x <- stringi::stri_encode(x, "", "UTF-8")
  x <- stringi::stri_trans_general(x, "name-any")
  max_encoding <- as.data.frame(table(Encoding(x)))
  max_encoding <- max_encoding$Var1[max_encoding$Freq == max(max_encoding$Freq) & max_encoding$Var1 !="unknown"]
  if(length(max_encoding)!=0) x <- iconv(x, from = as.character(max_encoding), to = "UTF-8")
  if (case == "upper"){
    x <- toupper(x)
  } else if(case == "lower"){
    x <- tolower(x)
  } else if (case == "title"){
    x <- stringr::str_to_title(x)
  } else if (case != "none"){
    stop("font case not recognised")
  }
  return(x)
}
