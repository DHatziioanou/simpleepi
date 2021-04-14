#' Title aggregate a dataframe or datatable by an id column
#'
#' @param dt data
#' @param idcol column with unique identifiers to aggregate by
#' @param disregard Optional; values to remove from aggregation eg NA, NULL, 0, -1. Default is NA.
#' @param prefkeep Optional; Where columns end up with more than one value should the first value be saved. Deafault is FALSE.
#' @param col Optional; Columns to process with prefkeep
#'
#' @return
#'
#' @examples
#'
#' messy <- data.frame(ID = c(1,1,2,3), data = c("red", "yellow", "red", "green"))
#' clean <- simpleaggregate(dt = messy, idcol = "ID", disregard=c(NULL,NA,""))
#'
#' @export
simpleaggregate <- function(dt, idcol, disregard =NA, prefkeep=FALSE, col){

  data.table::setDT(dt)

  # duplicates
  dup <- dt[duplicated(get(idcol))|duplicated(get(idcol), fromLast = T),]
  for (j in names(dup)) data.table::set(dup,which(dup[[j]] %in% disregard),j,NA)

  # Aggregate by idcol to comma separated strings
  dup <- dup[, lapply(.SD, function(x) toString(unique(stats::na.omit(x)))), by = idcol]

  # If values differ pick preference by order
  if(prefkeep != FALSE){
    if(length(col)==1){
      if(col == "all"){
        cols = names(dup)[1:ncol(dup)]
      }
    }
    dup[ , (cols) := lapply(.SD, function(x) {gsub("^(.*?),.*", "\\1", x)}), .SDcols = cols]
  }

  # Convert previous Date columns back to Date format
  Date_columns <- colnames(dt)[grepl("Date", sapply(dt,class))]
  if (isTRUE(length(Date_columns) > 0)) {
    dup[ , (Date_columns) := lapply(.SD, function(x) {simpledates(x)}), .SDcols = Date_columns]
  }

  dt <- dt[!(get(idcol) %in% dup[,get(idcol)]),]
  dt <- data.table::rbindlist(list(dt, dup), use.names = T, fill = T)

  data.table::setDF(dt)
  return(dt)
}
