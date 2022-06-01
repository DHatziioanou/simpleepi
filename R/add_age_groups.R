#' Title Add age group column to data.frame
#'
#' @param age  age vector
#' @param age_groups Input vector of age ranges with upper range marked as x+
#' @param intermediates values which could go up or down (under development)
#' @param unknowns vector of Age values to be considered unknowns eg -1, NA, "not stated"
#' @param factor
#'
#' @return Returns age groups
#'
#' @examples
#' # df <- data.frame(ID = c(1:4), Age = c(8,-1,38,26))
#' # age_groups <- c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+")
#' # df$group <- add_age_groups(df$Age, age_groups, unknowns, factor)
#'
#' @import data.table
#' @export
add_age_groups <- function(age, age_groups= c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+"), unknowns = c("-1", NA), factor = FALSE){
  if(missing(age)) stop("No age data")
  a <- data.table::data.table(do.call(rbind, base::strsplit(age_groups, "-", fixed=TRUE)))
  a[,V1 := as.numeric(gsub("+", "", V1, fixed = T))][,V2:=as.numeric(gsub("+", "", V2, fixed = T))][grep("+",age_groups, fixed = T),V2 := Inf]
  a <- cbind(a, age_groups)
  out <- sapply(age, FUN = function(x) a[V1 <= x & V2>=x,]$age_groups, simplify = TRUE)
  out[sapply(out,function(x) length(x)==0 | any(x %in% unknowns))]<- "unknown age"
  out <- unlist(out)
  if(isTRUE(factor)) out <- factor(out, c(age_groups, "unknown age"))
  return(out)
}



