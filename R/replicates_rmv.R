#' Remove duplicated rows by one or more columns
#'
#' @param df data
#'
#' @param columns column or vector of columns to look for duplicate combinations in
#'
#' @return Returns df without duplicated rows  based on the specified columns
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords duplicates, replicates
#'
#' @examples
#'
#' df_exclude_all_duplicates <- replicates_rmv(df, c("ID1","ID2"))
#'
#'
#' @export
replicates_rmv <- function(df,columns){
  df <- as.data.frame(df)
  x <- df[!(duplicated(df[,c(columns)]) | duplicated(df[,c(columns)], fromLast = T)),]
  return(x)
}
