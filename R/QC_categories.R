#' Title
#'
#' @param data vector to perform check on
#' @param cat categorical values to look for
#'
#' @return
#' @export
#'
#' @examples
#' 
#' check1 <- QC_categories(df$coolumn1, cat = c("low", "medium", "high"))
#' 
QC_categories <- function(data, cat){
  cat <- as.vector(cat)
  data <- as.vector(data)
  data_cat_N <- length(unique(data) %in% cat)
  QC <- ifelse(data_cat_N == length(cat), "TRUE - Identical",
               ifelse(data_cat_N > length(cat), "FALSE - Additional categories", "TRUE - Missing categories"))
  return(QC)
}