#' Title aggregate a data.frame or data.table to have 1 row per id column
#'
#' @param dt data
#' @param idcol column with unique identifiers to aggregate by
#' @param reformat Optional; TRUE (default) reformats to original column classes. FALSE returns all as character. Use FALSE to avoid losing different dates per ID.
#' @param disregard Optional; cell values to remove prior to aggregation
#' eg NA, NULL, 0, -1. Default; NA, "".
#' @param prefkeep Optional; Keep 1st record value where duplicate record values differ. Default; FALSE.
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
simpleaggregate <- function(dt, idcol, reformat = TRUE, disregard = c(NA, ""), prefkeep = FALSE, col=NULL){
  dt <- data.table::data.table(dt, stringsAsFactors = FALSE)
  # duplicates
  dup <- dt[base::duplicated(get(idcol)) | base::duplicated(get(idcol), fromLast = T), ]
  # remove "disregard" values
  for (j in names(dup)) data.table::set(dup,which(dup[[j]] %in% disregard),j,NA)
  # idcol aggregate to comma separated
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
  if(reformat){
    cl <- sapply(dt,class)
    dc <- colnames(dt)[grepl("Date", cl)]
    if (length(dc) > 0) { dup[ , (dc) := lapply(.SD, function(x) {simpledates(as.character(x))}), .SDcols = dc] }
    pc <- colnames(dt)[grepl("POSIXct", cl)]
    if (length(pc) > 0) { dup[ , (pc) := lapply(.SD, function(x) {as.POSIXct(as.character(x))}), .SDcols = pc] }
    oct <- colnames(dt)[grepl("octmode", cl)]
    if (length(oct) > 0) { dup[ , (oct) := lapply(.SD, function(x) {as.octmode(as.numeric(x))}), .SDcols = oct] }
    i64 <- colnames(dt)[grepl("integer64", cl)]
    if (length(i64) > 0) { dup[ , (i64) := lapply(.SD, function(x) {bit64::as.integer64(as.numeric(x))}), .SDcols = i64] }
    inorm <- colnames(dt)[cl=="integer"]
    if (length(inorm) > 0) { dup[ , (inorm) := lapply(.SD, function(x) {as.integer(as.numeric(x))}), .SDcols = inorm] }
    lg <- colnames(dt)[cl=="logical"]
    if (length(lg) > 0) { dup[ , (lg) := lapply(.SD, function(x) {as.logical(x)}), .SDcols = lg] }
    # Check all classes converted
    new_cl <- sapply(dup,class)
    dicol <- cl[cl != new_cl]
    if(length(dicol)>0) warning(paste("Column classes not covered;", paste(unique(dicol), collapse = ", ")))
  } else {
    dt[ , (names(dt)) := lapply(.SD, function(x) {as.character(x)}), .SDcols = names(dt)]
  }
  dt <- dt[!(get(idcol) %in% dup[,get(idcol)]),]
  dt <- data.table::rbindlist(list(dt, dup), use.names = T, fill = T)
  data.table::setDF(dt)
  return(dt)
}
