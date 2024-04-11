
#' Title Start connection to history and log files
#'
#' @param path Optional path to history and output files. Default is working directory.
#' @param history_file Optional file name for history. Default is file.path(path, paste0(date,"_Rhistory.txt"))
#' @param output_file  Optional file name for output. Default is file.path(path, paste0(date, "_Rlog.txt"))
#'
#' @return Opens connection to history and output files
#'
#' @examples
#' simplelog_start()
#'
#' # path = "getwd()"
#' # history_file <- file.path(path, paste0(Sys.Date(),"Rhistory.txt"))
#' # output_file <- file.path(path, paste0(Sys.Date(),"Rlog.txt"))
#' # simplelog_start(path, history_file, output_file)
#'
#' @export
simplelog_start <- function(path, history_file, output_file){

# Filenames
date <- format(Sys.Date(), "%Y%m%d")
if(missing(path)) {path = getwd()}
if(missing(history_file)) { history_file <- file.path(path, paste0(date,"_Rhistory.txt")) }
if(missing(output_file))  { output_file <- file.path(path, paste0(date, "_Rlog.txt")) }

# history_file connect
if(!file.exists(history_file)) cat(timestamp(),file=history_file,sep="\n", append = F)
loadhistory(history_file)
# output_file connect
sink(file = output_file, append = T, split = T, type = c("output", "message"))
timestamp()

}


#' Title Write data and end connection to history and log files
#'
#' @param path Optional path to history and output files. Default is working directory.
#' @param history_file Optional file name for history. Default is file.path(path, paste0(date,"_Rhistory.txt"))
#' @param output_file  Optional file name for output. Default is file.path(path, paste0(date,"_Rlog.txt"))
#'
#' @return  Closes connection to history and output files
#'
#' @examples
#' # simplelog_end(path, history_file, output_file)
#'
#' @export
simplelog_end <- function(path, history_file, output_file){

  # Filenames
  date <- format(Sys.Date(), "%Y%m%d")
  if(missing(path)) {path = getwd()}
  if(missing(history_file)) { history_file <- file.path(path, paste0(date,"Rhistory.txt")) }
  if(missing(output_file))  { output_file <- file.path(path, paste0(date, "Rlog.txt")) }

  timestamp()
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
  savehistory(file = history_file)

}

