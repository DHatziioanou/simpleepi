#' Title Add age group column to data.frame
#'
#' @param age  age vector
#' @param groups Input vector of age ranges in format "N1-N2" where N1 is the lower age and N2 the upper age. Mark range where max is infinite as "N1+"
#' @param unknowns vector of Age values to be considered unknowns eg -1, NA, "not stated"
#' @param factor
#'
#' @return Returns age groups
#'
#' @examples
#' # df <- data.frame(ID = c(1:4), Age = c(8,-1,38,26))
#' # groups <- c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+")
#' # df$group <- age_groups(df$Age, groups, unknowns, factor)
#'
#' @import data.table
#' @export
age_groups <- function(age, groups= c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+"), unknowns = c("-1", NA), factor = FALSE){
  if(missing(age)) stop("No age data")
  a <- data.table::tstrsplit(groups, "-", fill=NA, type.convert=F, names=c("L","R"), fixed=TRUE)
  a <- data.table::data.table(L = a$L, R = a$R)
  a[,L := as.numeric(gsub("+", "", L, fixed = T))][,R:=as.numeric(gsub("+", "", R, fixed = T))][grep("+",groups, fixed = T),R := Inf]
  a <- cbind(a, groups)
  out <- sapply(age, FUN = function(x) a[L <= x & R>=x,]$groups, simplify = TRUE)
  out[sapply(out,function(x) length(x)==0 | any(x %in% unknowns))]<- "unknown age"
  out <- unlist(out)
  if(isTRUE(factor)) out <- factor(out, c(groups, "unknown age"))
  return(out)
}



