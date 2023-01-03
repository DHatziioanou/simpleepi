#' Subset data.frame or data.table for all rows duplicated in one or more columns
#'
#' @param df data.frame or data.table
#' @param columns column or vector of columns to look for duplicate combinations in
#'
#' @return Returns duplicated rows within df based on the specified columns as data.frame
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords duplicates, replicates
#'
#' @examples
#' # df_duplicates <- replicates(df, c("Name","DOB"))
#'
#'
#' @export
replicates <- function(df,columns){
  df <- as.data.frame(df)
  x <- df[duplicated(df[,c(columns)]) | duplicated(df[,c(columns)], fromLast = T),]
  return(x)
}


#' Subset data.frame or data.table to remove all duplicated rows by one or more columns
#'
#' @param df data.frame or data.table
#' @param columns column or vector of columns to look for duplicate combinations in
#'
#' @return Returns data.frame without duplicated rows based on the specified columns
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords duplicates, replicates
#'
#' @examples
#'
#' # df_exclude_all_duplicates <- replicates_rmv(df, c("ID1","ID2"))
#'
#'
#' @export
replicates_rmv <- function(df,columns){
  df <- as.data.frame(df)
  x <- df[!(duplicated(df[,c(columns)]) | duplicated(df[,c(columns)], fromLast = T)),]
  return(x)
}

#' Title Combine rows sharing unique identifier to save all unique data to 1 row per identifier
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
#' data <- c("red", "yellow", "red", "green"))
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

#' Combine tables from environment into one table by appending rows
#'
#' @param x data.frame or data.table to append
#' @param name name to call database of appended x
#'
#' @return object named with name argument in .GlobalEnv with data from x appended at end
#'
#' @examples
#' for (v in 1:3){
#'   df <- data.frame(a = stats::runif(3), b = c("a", "b", "c"))
#'   df$version <- v
#'   simpleappendenv(x = df, name = "DF")
#' }
#'
#' @export
simpleappendenv <- function(x, name= "DF"){
  if(exists(name)) {
    base::assign(x = name, value = data.table::rbindlist(list(get(name),x), use.names = T,fill = T) , envir = .GlobalEnv)
  } else {
    base::assign(x = name, value = x, envir = .GlobalEnv)
  }
  return(get(name))
}



