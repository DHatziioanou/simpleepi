#' Title Compare two versions of data with the same length and mark new data in a version control column
#'
#' @param dt Input data
#' @param id Unique ID column
#' @param oldcol Column of historic data
#' @param newcol Column of updated data
#' @param vccol existing or new column capturing version control
#' @param olddate date of previous data for version control
#' @param newdate  date of new data for version control
#' @param type one of "list" or "flat"
#' @param out one of "table" or "vector" ro NULL. NULL returns table and writes to as rds
#'
#' @return returns vccol newdate with new changes; records 1st record per ID which is not NA then adds any changes to the oldcol values. In list format this is by adding new data to new rows for each ID and for flat format data in added to vccol in format olddate;oldcol and any changes are added as newdate;newcol
#'
#' @examples
#' # dt <- data.table::data.table(c(1:6), c("A", "B", "C", NA, "D", ""), c("", "Test", NA, NA, "C", NA))
#' # dt$VC <- simple_version_control(dt,
#' #   oldcol = "V2", newcol = "V3", id = "V1",
#' #   olddate = "old", newdate = "new",
#' #   type = "flat",
#' #   out = "vector")
#'
#' # dt$VC <- simple_version_control(dt,
#' #    oldcol = "V2", newcol = "V3", id = "V1",
#' #    olddate = "old", newdate = "new",
#' #    type = "list",
#' #    out = "vector")
#'
#' @import data.table
#' @export
simple_version_control <- function(dt, id, oldcol, newcol, vccol = NULL, olddate = "original", newdate = Sys.Date(), type = "list", out = NULL){
start <- Sys.time()

# Input checks
if(!any(class(dt) %in% "data.table")) stop("dt not a data.table")
if(!oldcol %in% names(dt)) stop(paste0("oldcol not found"))
if(!newcol %in% names(dt)) stop(paste0("newcol not found"))
if(!any(type %in% c("list","flat"))) stop("invalid type")
if(!(out %in% c("table", "vector", "file"))) warning("invalid out, will write to disk")
if(sum(duplicated(dt[[(id)]]))>0) stop("id is not unique")

# Prep data
  # Make VC column
if(missing(vccol)|is.null(vccol)) {
  cols = c(id, oldcol, newcol)
  a <- dt[, ..cols ]
  a[ get(oldcol) == "", (oldcol):= NA][ get(newcol) == "", (newcol):= NA]
  vccol = paste0(oldcol,"_VC")
  if(type == "list"){
    a = a[, .(VC = list(data.table::data.table(date = olddate, versions = get(oldcol), notes = vector(mode = "character", length = 1)))), by = list(get(id),get(oldcol),get(newcol))][ ]
   setnames(a, c("get", "get.1", "get.2", "VC"), c(id, oldcol, newcol, "VC"))
   message(paste("made version control list column", vccol))
  } else if (type == "flat") {
   a[, VC := paste(olddate, get(oldcol), sep = ";")]
   a[is.na(get(oldcol)), VC := ""]
   message(paste("made version control column", vccol))
} else {
  # Use existing VC column
  a <- data.table::copy(dt[, .SD, .SDcols = c(id, oldcol, newcol, vccol)])
  a[ get(oldcol) == "", (oldcol):= NA][ get(newcol) == "", (newcol):= NA]
  setnames(a, c(vccol), c("VC"))
}
}

# Version control
if(type == "list"){
 # Add new rows to list column
  a = a[ , .(VC = data.table::fifelse( !(is.na(get(oldcol)) & is.na(get(newcol))),
           data.table::fifelse( ( !is.na(get(oldcol))&is.na(get(newcol)) )  | ( is.na(get(oldcol))&!is.na(get(newcol)) ) | ( get(oldcol)!=get(newcol) ) ,
    lapply(VC, function(x) rbindlist(list(x, data.table::data.table(date = as.character(newdate), versions = get(newcol), notes = vector(mode = "character", length = 1))), use.names = F)), VC), VC)), by = list(get(id), get(oldcol), get(newcol))]
setnames(a, c("get", "get.1", "get.2", "VC"), c(id, oldcol, newcol, "VC"))

} else if(type == "flat"){
  # Add new to end of column
    a =a[, VC := data.table::fifelse(is.na(get(oldcol)) & is.na(get(newcol)), VC,
                          data.table::fifelse(is.na(get(oldcol)) & !is.na(get(newcol)),
                                  data.table::fifelse(VC ==""|is.na(VC),paste(newdate, get(newcol), sep = ";"),
                                          paste(VC, paste(newdate, get(newcol), sep = ";"), sep = ",")),
                                  data.table::fifelse(!is.na(get(oldcol)) & is.na(get(newcol)), paste(VC, paste(newdate, get(newcol), sep = ";"), sep = ","),
                                          data.table::fifelse(get(oldcol) != get(newcol), paste(VC, paste(newdate, get(newcol), sep = ";"), sep = ","),
                                                  VC)))), by = get(id)]
}

  data.table::setnames(a, "VC", vccol)
  end <- Sys.time(); runtime = end -start
  cat(paste(vccol, "version control took", round(runtime, 2), attr(runtime, "units")))
  # Output
  if(out == "table") {
    return(a)
  } else if(out == "vector") {
    return(a[[vccol]])
  } else {
    saveRDS(object = a, file = paste0(vccol, ".rds"), ascii = FALSE, compress	= "gzip", version = "3")
    message(paste0("Saved "), paste0(vccol, ".rds"))
    return(a)
  }
}
