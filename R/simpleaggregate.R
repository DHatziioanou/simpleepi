

#' Title
#'
#' @param dt data
#' @param idcol column with unique identifiers to aggregate by
#' @param disregard Optional; values to remove from aggregation eg NA, NULL, 0, -1. Default is NA.
#' @param prefkeep Optional; Numeric value representing which comma separated value to keep where aggregation results in more than one comma separated values. Default is to keep all values.
#'
#' @return
#'
#' @examples
#' 
#' @export
simpleaggregate <- function(dt, idcol, disregard, prefkeep){

  data.table::setDT(dt)
  # replace "" with NA or "disregard" values
  if(!exists(disregard)) disregard <- NA
  for (j in names(dt)) data.table::set(dt,which(dt[[j]] %in% disregard),j,NA)
  
  # Aggregate by idcol to comma separated strings
  dt <- dt[, lapply(.SD, function(x) toString(unique(data.table::na.omit(x)))), by = get(idcol)]
  
  # If values differ pick preference by order
  cols = names(dt)[1:ncol(dt)] 
  if(!exists(prefkeep)){
  dt[ , (cols) := lapply(.SD, function(x) {gsub("^(.*?),.*", paste0("\\",prefkeep), x)}), .SDcols = cols] 
  }
  
  # fix dates
  cols = names(dt)[grepl("date", names(dt), ignore.case = T)] 
  dt[ , (cols) := lapply(.SD, function(x) {as.Date(x, "%Y-%m-%d")}), .SDcols = cols] 
  
  data.table::setDF(dt)
  return(dt)
}