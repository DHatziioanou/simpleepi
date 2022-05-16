#' Title aggregate a data.frame or data.table to have 1 row per id column
#'
#' @param dt data
#' @param idcol column with unique identifiers to aggregate by
#' @param disregard Optional; values to remove from aggregation eg NA, NULL, 0, -1. Default is NA.
#' @param prefkeep Optional; Where columns end up with more than one value should the first value be saved. Default is FALSE.
#' @param col Optional; Columns to process with prefkeep. "all" will process all columns
#'
#' @return Returns data.frame aggregated to one row per idcol.
#' @import data.table
#'
#' @examples
#' messy <- data.frame(ID = c(1,1,2,3),
#'   data = c("red", "yellow", "red", "green"))
#' clean <- simpleaggregate(
#'    dt = messy,
#'    idcol = "ID",
#'    disregard=c(NULL,NA,""))
#' clean <- simpleaggregate(dt = messy,
#'    idcol = "ID",
#'    disregard=c(NULL,NA,""),
#'    prefkeep = 1,
#'    col = "all")
#'
#' @export
simpleaggregate <- function(dt, idcol, disregard =NA, prefkeep=FALSE, col){

  dt <- data.table::data.table(dt, stringsAsFactors = FALSE)

  # duplicates
  dup <- dt[base::duplicated(get(idcol)) | base::duplicated(get(idcol), fromLast = T), ]
  for (j in names(dup)) data.table::set(dup,which(dup[[j]] %in% disregard),j,NA)

  # Aggregate by idcol to comma separated strings
  dup <- dup[, lapply(.SD, function(x) toString(unique(stats::na.omit(x)))), by = idcol]

  # If values differ pick preference by order
  if(prefkeep != FALSE){
    if(length(col)==1){
      if(col == "all"){
        col = names(dup)[1:ncol(dup)]
      }
    }
    dup[ , (col) := lapply(.SD, function(x) {gsub("^(.*?),.*", "\\1", x)}), .SDcols = col]
  }

  # Convert previous Date columns back to Date format
  Date_columns <- colnames(dt)[grepl("Date", sapply(dt,class))]
  if (isTRUE(length(Date_columns) > 0)) {
    dup[ , (Date_columns) := lapply(.SD, function(x) {as.character(x)}), .SDcols = Date_columns]
    dup[ , (Date_columns) := lapply(.SD, function(x) {simpledates(x)}), .SDcols = Date_columns]
  }
  Posix_columns <- colnames(dt)[grepl("POSIXct", sapply(dt,class))]
  if (isTRUE(length(Posix_columns) > 0)) {
    dup[ , (Posix_columns) := lapply(.SD, function(x) {as.character(x)}), .SDcols = Posix_columns]
    dup[ , (Posix_columns) := lapply(.SD, function(x) {as.POSIXct(x)}), .SDcols = Posix_columns]
  }
  octmode_columns <- colnames(dt)[grepl("octmode", sapply(dt,class))]
  if (isTRUE(length(octmode_columns) > 0)) {
    dup[ , (octmode_columns) := lapply(.SD, function(x) {as.numeric(x)}), .SDcols = octmode_columns]
    dup[ , (octmode_columns) := lapply(.SD, function(x) {as.octmode(x)}), .SDcols = octmode_columns]
  }
  dt <- dt[!(get(idcol) %in% dup[,get(idcol)]),]
  dt <- data.table::rbindlist(list(dt, dup), use.names = T, fill = T)

  data.table::setDF(dt)
  return(dt)
}
