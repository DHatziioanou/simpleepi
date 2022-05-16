#' Title Add age group column to data.frame
#'
#' @param df  data.frame with an age column
#' @param age_column column from which to calculate ages
#' @param age_groups Input vector of age ranges with upper range marked as x+
#' @param intermediates values which could go up or down (under development)
#' @param unknowns vector of Age values to be considered unknowns eg -1, NA, "not stated"
#' @param factor
#'
#' @return Returns input data.frame with added age group column
#'
#' @examples
#'
#' df <- data.frame(ID = c(1:4), Age = c(8,-1,38,26))
#' unknowns <- c(-1, NA)
#' age_groups <- c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+")
#' age_column <- "Age"
#' factor <- FALSE
#' intermediates <- "down"
#' add_age_groups(df, age_column, age_groups, intermediates, unknowns, factor)
#'
#' @export
add_age_groups <- function(df, age_column, age_groups, intermediates, unknowns, factor){
  if(missing(intermediates)) intermediates <- "down"
  if(missing(unknowns)) unknowns <- c("-1", NA)
  if(missing(factor)) factor <- FALSE
  if(missing(age_groups)) age_groups <- c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+")
  if(missing(df)) stop("No data selected")
  if(missing(age_column)) stop("age_column")

#  if(intermediates == "down")

  a <- data.frame(do.call(rbind, base::strsplit(age_groups, "-", fixed=TRUE)))
  a$X1[grepl("+",a$X1, fixed = T)] <- gsub("+", "",  a$X1[grepl("+",a$X1, fixed = T)], fixed = T)
  a$X2[grepl("+",a$X2, fixed = T)] <- Inf
  a <- do.call(cbind, lapply(a, function(x) as.numeric(as.character(x))))
  df$`Age group` <- as.character(NA)

  for (i in 1:nrow(a)) {
 df$`Age group`[df[,age_column] >= as.numeric(a[i,1]) & df[,age_column] <= as.numeric(a[i,2])] <- age_groups[i]
   }
  df$`Age group`[is.na(df[,age_column]) | df[,age_column] %in% unknowns] <- "unknown age"
  if(isTRUE(factor)) df$`Age group` <- factor(df$`Age group`, c(age_groups, "unknown age"))
  return(df)
}
