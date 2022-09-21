#' Title Compare two versions of data with the same length and mark new data in a version control column
#'
#' @param dt Input data
#' @param id Unique ID column
#' @param oldcol Column of historic data
#' @param newcol Column of updated data
#' @param type one of "list" or "flat"
#' @param out one of "table" or "vector" or NULL. NULL returns table and writes to rds file
#' @param vccol Optional; name of existing version control column or name to assign to new one. If missing default uses common part of oldcol and newcol strings followed by _VC or uses oldcol if no common string.
#' @param olddate Optional; date of previous data. Default is "original".
#' @param newdate  Optional; date of new data. Default is system date.
#'
#' @return returns vccol newdate with new changes; records 1st record per ID which is not NA then adds any changes to the oldcol values. In list format this is by adding new data to new rows for each ID and for flat format data in added to vccol in format olddate;oldcol and any changes are added as newdate;newcol
#'
#' @examples
#' # dt <- data.table(c(1:6),
#' #             c("A", "B", "C", NA, "D", ""),
#' #             c("", "Test", NA, NA, "C", NA))
#' # dt$VC <- simple_version_control(dt,
#' #   oldcol = "V2", newcol = "V3", id = "V1",
#' #   olddate = "old", newdate = "new",
#' #   type = "flat",
#' #   out = "vector")
#' #
#' # dt$VC <- simple_version_control(dt,
#' #    oldcol = "V2", newcol = "V3", id = "V1",
#' #    olddate = "old", newdate = "new",
#' #    type = "list",
#' #    out = "vector")
#' #
#' # Process by chunk
#' # ds <- split(dt, (as.numeric(rownames(dt))-1) %/% 10000000)
#' # for (s in 1:length(ds)){
#' # ds[[s]][, variable_VC :=
#' #         simple_version_control(dt = ds[[s]],
#' #                id = "key",
#' #                oldcol = "valueprev",
#' #                newcol = "value",
#' #                olddate = "20220503",
#' #                newdate = "20220510",
#' #                type = "flat",
#' #                out = "vector",
#' #                vccol = "variable_VC")]
#' # }
#' # ds <- rbindlist(ds, use.names = T, fill = T)
#'
#' @import data.table
#'
#' @export
simple_version_control <- function(dt, id, oldcol = NULL, newcol, type = "list", out = NULL, vccol, olddate = NULL, newdate = Sys.Date()){
  start <- Sys.time()

  # Input checks
  if(!any(class(dt) %in% "data.table")) stop("dt not a data.table")
  if(!is.null(olddate)) if(!oldcol %in% names(dt)) stop(paste0("oldcol not found in dt"))
  if(!newcol %in% names(dt)) stop(paste0("newcol not found"))
  if(!(missing(vccol))) if(!(vccol %in% names(dt))) stop(paste0("vccol not found in dt"))
  if(!any(type %in% c("list","flat"))) stop("invalid argument type use list or flat")
  if(!(out %in% c("table", "vector", "file"))) warning("invalid argument out, will write to disk")
  if(sum(duplicated(dt[[(id)]]))>0) stop("id is not unique")
  size <- as.numeric(gsub("[[:alpha:]]", "", format(object.size(dt), units = "Mb")))
  if(as.numeric(gsub("[[:alpha:]]", "", size)) >= 300) warning(paste0("Large input of ", size, " MB, split to chunks if process fails"))


  # 1st run 1db
  if(is.null(oldcol) & is.null(olddate)) {


    # Record version 1

    # Make VC column
    if(missing(vccol)) {
      vccol <- paste0(newcol, "_VC")
    }
    # Process
    cols = c(id, newcol)
    a <- dt[, ..cols ]
    a[ get(newcol) == "", (newcol):= NA]
    if(type == "list"){
      a = a[, .(VC = list(data.table::data.table(date = newdate, versions = get(newcol), notes = vector(mode = "character", length = 1)))), by = list(get(id),get(newcol))][ ]
      data.table::setnames(a, c("get", "get.1", "VC"), c(id, newcol, "VC"))
      message(paste("made version control list column", vccol))
    } else if (type == "flat") {
      a[, VC := paste(newdate, get(newcol), sep = ";")]
      a[is.na(get(newcol)), VC := ""]
      message(paste("made version control column", vccol))
    }

  } else {

    # Compare 2 versions

    # Make VC column if missing
    if(missing(vccol)) {
      if(!exists("vccol")) {
        pat <- paste(qualV::LCS(strsplit(as.character(oldcol), '')[[1]], strsplit(as.character(newcol), '')[[1]])$LCS,collapse = "")
        vccol <- paste0(ifelse(pat=="", oldcol, pat),"_VC")
      }
      cols = c(id, oldcol, newcol)
      a <- dt[, ..cols ]
      a[ get(oldcol) == "", (oldcol):= NA][ get(newcol) == "", (newcol):= NA]
      if(type == "list"){
        a = a[, .(VC = list(data.table::data.table(date = olddate, versions = get(oldcol), notes = vector(mode = "character", length = 1)))), by = list(get(id),get(oldcol),get(newcol))][ ]
        data.table::setnames(a, c("get", "get.1", "get.2", "VC"), c(id, oldcol, newcol, "VC"))
        message(paste("made version control list column", vccol))
      } else if (type == "flat") {
        a[, VC := paste(olddate, get(oldcol), sep = ";")]
        a[is.na(get(oldcol)), VC := ""]
        message(paste("made version control column", vccol))
      }} else {
        # Use existing VC column
        a <- dt[, .SD, .SDcols = c(id, oldcol, newcol, vccol)]
        a[ get(oldcol) == "", (oldcol):= NA][ get(newcol) == "", (newcol):= NA]
        data.table::setnames(a, c(vccol), c("VC"))
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

  }

  # Create attributes

  data.table::setattr(a$VC, "Processed", append(x = attributes(a$VC)$Processed, values = Sys.time(), after = F))
  data.table::setattr(a$VC, "versions", append(x = attributes(a$VC)$versions, values = newdate, after = F))
  data.table::setnames(a, "VC", vccol)
  end <- Sys.time(); runtime = end -start
  cat(paste(vccol, "version control took", round(runtime, 2), attr(runtime, "units")))


  # Return Output

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



#' Title Get a snapshot of previous database contents
#'
#' @param x Flat version control column made from simple_version_control
#' @param date date of snapshot
#'
#' @return
#'
#' @examples
#' # date <- "20220529"
#' # col <- "column_VC"
#' # df$Historic20220529 <- simple_vc_snapshot(x = df$data_VC, date = "20220529")
#' @import data.table
#' @export
simple_vc_snapshot <- function(x = test$genotyping_variant_VC, date){
  vc <- attributes(x)$versions
  if(!any(date %in% vc)) stop(paste0("no version control for ",date,". Use attributes(x) for available dates"))
  fut <- vc[simpledates(vc) > simpledates(date)]
  y <- strsplit(as.character(x), ',')
  out <- sapply(y, FUN = function(x) {
    if(length(x)==0){
      x <- NA
    } else if(all(length(x)==1 & is.na(x))) {
      x <- NA
    } else{
      d <- grep(date,x,fixed = T)
      if(length(d)>0) {
        x <- x[d]
      } else {
        # remove fut
        f <- grep(paste0(fut,collapse = "|"),x,fixed = T)
        if(length(f)>0) {
          x <- x[-f]
          if(length(x)==0) x <- NA
        }
        # get last
        x <-x[length(x)]
      }
      if(!is.na(x)) x <- sapply(strsplit(x, split = ";"), tail, 1)
    }
  }, simplify = FALSE)
  out[sapply(out,function(x)is.null(x))] <- NA
  out <- unlist(out)
  out[grep("NA",out)]<-NA
  return(out)
}

#' Title Create and maintain a record of the number of rows in an input with each combination of values from columns of interest
#'
#' @param logpath file path where log is saved.
#' @param logname string containing descriptive name of log. Processing date will be added to this after creation.
#' @param input Cumulative dataset to be processed.
#' @param columns columns within input with value combinations to quantify.
#' @param inputname Identifier of new data such as file name or update date
#' @param Date Optional; date of updated data. Default is system date.
#'
#' @return
#'
#' @examples
#'
#' @import data.table
#' @export
file_stat_log <- function(logpath = NULL, logname = NULL, input, inputname, columns, Date = Sys.Date()){

  if(!all(columns %in% names(input))) stop(paste("columns", paste0(columns[!columns %in% names(input)], collapse = ", "), "not in", deparse(substitute(input))))

  # Look for last log
  lastlog <- try(simpleepi::getlatestfile(folder_path = logpath, logname, return_type = "path", maxTries = 10))
  # Make db in 1st run or import last if exists
  if(class(lastlog) == "try-error"){
    cat(paste0("Previous log not found, initiating new log ", format( Sys.Date(), "%Y%m%d"), logname))
    logs = data.table(input = character(),
                      Date = character())
    logs[,columns] <- character()
    logs[,"Count"] <- integer()
  } else {
    logs = data.table::fread(lastlog, header = T, stringsAsFactors = F, showProgress = T, na.strings = c("NA", "NULL", NULL), encoding = "UTF-8")
    logs[,Date := simpledates(Date)]
  }

  # New stats
  s <- input[,.(input = inputname, Date = simpledates(Date), Count = .N),by = columns]

  # Combine and write stats
  newlog <- data.table::rbindlist(list(logs, s), use.names = T, fill = T)
  if(!is.null(logname)){
    data.table::fwrite(newlog, file =  file.path(logpath, paste0(format( Sys.Date(), "%Y%m%d"),logname)),
                       row.names = F, col.names = T, append = F)
  }
  return(newlog)
}
#' Title
#'
#' @param logs log created using file_stat_log
#' @param columns  columns within log with quantified value combinations
#' @param logpath Path to log
#' @param logname Name of log
#' @param maxup Optional; Maximum permissible increase from previous log. Warning recorded if exceeded. Default is 1.25
#' @param maxdown Optional; Maximum permissible decrease from previous log. Warning recorded if exceeded. Default is 0.
#' @param Date  Optional; date to give processed log. Default is system date.
#'
#' @return
#'
#' @examples
#'
#' @import data.table
#' @export
file_stat_diff <- function(logs = file_stat_log, columns, logpath = NULL, logname = NULL, maxup = 1.25, maxdown = 0, Date = Sys.Date()){
  logs[order(Date),]
  logs[, diff := (Count - shift(x = Count, n =1L, fill=0, type = "lag")), by = columns]
  # percent change from one period to the next
  logs[, ROC := (Count - shift(x = Count, n =1L, fill=0, type = "lag")) / (shift(x = Count, n =1L, fill=0, type = "lag")) * 100 , by = columns][is.infinite(ROC),ROC:=NA]
  logs[, Warning := ifelse(Count!=diff & (Count <  ((Count-diff) * maxdown) | Count > ((Count-diff) * maxup)), TRUE, FALSE)]

  if(!is.null(logname)){
    data.table::fwrite(logs, file =  file.path(logpath, paste0(format(simpledates(Date), "%Y%m%d"),logname)),
                       row.names = F, col.names = T, append = F)
  }
  return(logs)
}


