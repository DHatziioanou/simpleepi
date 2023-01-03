#' Compare values or vectors
#'
#' @param x vector to compare (left)
#' @param y vector to comapre (right)
#' @param simple TRUE returns logical TRUE (identical) or FALSE (any difference). FALSE returns descriptive string of comparison outcome.
#'
#' @return returns comparison in logical or text form depending on simple argument
#'
#' @examples
#' # df$colsame <- same_as(df$col1, df$col2, simple = F)
#' # df$colsame <- same_as(df$col1, df$col2, simple = T)
#'
#'
#' @export
same_as <- function(x,y, simple = TRUE){
  test <- data.frame(x = x, y =y)
  test$x[test$x==""] <- NA
  test$y[test$y==""] <- NA
  if(simple){
    test$out <- ifelse(test$x==test$y, TRUE, FALSE)
    test$out[(is.na(test$x) & is.na(test$y)) | (test$x=="" & test$x=="") | (is.na(test$x) & test$y=="") |(test$x == "" & is.na(test$y))] <- TRUE
    test$out[is.na(test$x) & !is.na(test$y)] <-  FALSE
    test$out[!is.na(test$x) & is.na(test$y)] <-  FALSE
  } else {
    test$out <- ifelse(test$x==test$y, "identical", "mismatch")
    test$out[(is.na(test$x) & is.na(test$y)) | (test$x=="" & test$x=="") | (is.na(test$x) & test$y=="") |(test$x == "" & is.na(test$y))] <- "Both missing"
    test$out[is.na(test$x) & !is.na(test$y)] <-  "Missing left"
    test$out[!is.na(test$x) & is.na(test$y)] <-  "Missing right"
  }
  return(test$out)
}
