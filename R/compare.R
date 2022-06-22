#' Title
#'
#' @param x
#' @param y
#' @param simple
#'
#' @return
#'
#' @examples
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
    test$out[is.na(test$x) & !is.na(test$y)] <-  "Missing right"
    test$out[!is.na(test$x) & is.na(test$y)] <-  "Missing left"
  }
  return(test$out)
}
