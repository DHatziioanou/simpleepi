#' Columns aggregated with non unique comma separated
#'
#' Aggregate a dataset such as a line list by a number of grouping variables and comma separate non-unique values in the rest
#'
#' @param df data
#'
#' @param columns column or vector of columns to group by
#'
#'
#' @return Returns df aggregated by the grouping columns with non unique values comma separated
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords aggregate, replicates
#'
#' @examples
#' df <- remove_replicates(df, c("Name","DOB"))
#'
#' df <- remove_replicates(df, c("Name","DOB", "episode"))
#'
#' @export
remove_replicates <- function(df, columns){
  df <- stats::aggregate(x =  df[,c(!(names(df) %in% columns))], by = df[,c(columns)], FUN = function(x) toString(x[!is.na(x)]), simplify = F)
  return(df)
}

