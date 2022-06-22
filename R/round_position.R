#' Title Round large numbers up or increase by certain value at defined position
#'
#' @param x numbers
#' @param fromleft position within number from left to round up
#'
#' @return x rounded up at defined position
#'
#' @examples
#'
#' x <- c(290, 5218, 462964)
#' y_max <- axis_number(max(x))
#'
#' @export
axis_number <- function(x, fromleft = 2, value = 1){
  if(fromleft==1) stop("not supported yet")
  if(any(round(x) != x)) warning("decimal places not supported")
  # Round digit up
  x <- floor(x)
  y <- as.numeric(substr(x,fromleft,fromleft))+value
  n <- vector(mode = "character",length = length(x))
  n[x==0] <- "0"
  # Round at position
  ru <- which(nchar(y)==1& n!="0")
  n[ru]<- as.numeric(paste0(substr(x[ru],1,fromleft-1),y[ru]))*`^`(10, nchar(x[ru])-fromleft)
  # Round previous position
  r1 <- which(nchar(y)==2& n!="0")
  n[r1]<- (as.numeric(substr(x[r1],1,fromleft-1))+(0.1*y[r1]))*`^`(10, nchar(x[r1])-fromleft+1)
  rn <- which(n=="")
  n[rn] <- x[rn]
  n <- as.integer(n)
  return(n)
}
