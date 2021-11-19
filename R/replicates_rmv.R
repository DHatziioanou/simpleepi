#' Retrieve duplicated rows by one or more columns
#'
#' Retrieve duplicated rows by one or more columns
#'
#' @param df data
#'
#' @param columns column or vector of columns to look for duplicate combinations in
#'
#' @return Returns duplicated rows within df based on the specified columns
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords duplicates, replicates
#'
#' @examples
#'
#' df_duplicates <- replicates(df, c("Name","DOB"))
#'
#'
#' @export
replicates_rmv <- function(df,columns){
  df <- as.data.frame(df)
  x <- df[!(duplicated(df[,c(columns)]) | duplicated(df[,c(columns)], fromLast = T)),]
  return(x)
}
