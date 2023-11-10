
#' Title Start connection to history and log files
#'
#' @param path Path to history and output files
#' @param history_file file name for history
#' @param output_file  file name for output
#'
#' @return Opens connection to history and output files
#'
#' @examples simplelog_start()
#' @export
simplelog_start <- function(path, history_file, output_file){

# Filenames
date <- format(Sys.Date(), "%Y%m%d")
if(missing(history_file)) {path = getwd()}
if(missing(history_file)) { history_file <- file.path(path, paste0(date,"Rhistory.txt")) }
if(missing(output_file))  { output_file <- file.path(path, paste0(date, "Rlog.txt")) }

# history_file connect
if(!file.exists(history_file)) cat(timestamp(),file=history_file,sep="\n", append = F)
loadhistory(history_file)
# output_file connect
sink(file = output_file, append = T, split = T, type = c("output", "message"))
timestamp()

}


#' Title Write data and end connection to history and log files
#'
#' @param path
#' @param history_file
#' @param output_file
#'
#' @return
#'
#' @examples
#' @export
simplelog_end <- function(path, history_file, output_file){

  # Filenames
  date <- format(Sys.Date(), "%Y%m%d")
  if(missing(path)) {path = getwd()}
  if(missing(history_file)) { history_file <- file.path(path, paste0(date,"Rhistory.txt")) }
  if(missing(output_file))  { output_file <- file.path(path, paste0(date, "Rlog.txt")) }

  timestamp()
  unlink(output_file)
  savehistory(file = history_file)

}
