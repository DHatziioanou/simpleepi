#' Check if two files are present and of the same size and same creation and modification time
#'
#' @param x file 1 to check
#' @param y file 2 to check
#'
#' @return  TRUE if both files exist, FALSE if the files are not identical
#'
#' @examples
#'
#' # simplewrite(list("test"), "file1.csv")
#' # simplewrite(list("test"), "file2.csv")
#' # simplefilecheck("file1.csv", "file2.csv")
#'
#' @export
simplefilecheck <- function(x,y){
  # 1st file exists
  if(!isTRUE(file.exists(x))){
    message(paste0(x," not found"))
    return(FALSE)
    stop()
  }
  # 2nd file exists
  if(!isTRUE(file.exists(y))){
    message(paste0(y," not found"))
    return(FALSE)
    stop()
  }
  yi <- base::file.info(y)
  data.table::setDT(yi, keep.rownames = TRUE)[]
  xi <- base::file.info(x, extra_cols=T)
  data.table::setDT(xi, keep.rownames = TRUE)[]
  # size
  if(yi$size != xi$size){
    message(paste0("files are different sizes"))
    return(FALSE)
    stop()
  }
  # creation
  if(yi$ctime != xi$ctime){
    message(paste0("files created at different times"))
    return(FALSE)
    stop()
  }
  # modification
  if(yi$mtime != xi$mtime){
    message(paste0("files modified at different times"))
    return(FALSE)
    stop()
  }
  return(TRUE)
}

#' Retrieve most recent file from a file path
#'
#' Retrieves the latest file name and modification details  for a specified path with the option to only look include files with a defined name string and exclude files with other defined name strings. The function can be set to attempt file retrieval a defined number of times to overcome server connectivity problems.
#'
#' @param folder_path Folder path containing the file to retrieve. Folders within this path will not be searched.
#'
#' @param file_string Character string within the file names to retrieve (optional). If not defined all file names within the folder are retrieved.
#'
#' @param exclusions Character string within the file names for file exclusion (optional). If not defined nothing is excluded from the retrieved file name list.
#'
#' @param return_type Use "all" to retrieve a data.frame with the file name, file path and modiciation date, "name" to retrieve the file name or "path" to retrieve the full file path. Default is "path". (optional)
#'
#' @param maxTries Maximum number of times to attempt file information retrieval. Default is 1. (optional)
#' @param include.dirs list.files argument; Logical; return folders TRUE for yes, FALSE for no. Default is no.
#' @param recursive list.files argument; Look in subdirectories
#'
#' @return Returns the last ctime file path, file name or a data.frame with the file name, path and modification details as defined by return_type where size is the file size in bytes, mtime is the last modification time, ctime is last status change time, atime is last access time.
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords file name, file path
#'
#' @examples
#' # Import latest file
#' # todays_file <- getlatestfile(file.path("...","subfolder","subfolder"),
#' #    return_type = "all", file_string = "epi", exclusions = "unwanted")
#' # df <- simpleimport(todays_file$path)
#'
#' # Record the modification details
#'  # Data_as_of <- getlatestfile(folder_path = file.path("...","subfolder","subfolder"),
#'  #  file_string = "csv",exclusions = "unwanted",
#'  #  return_type = "all", maxTries = 5)$ctime
#'
#' @export
getlatestfile <- function(folder_path, file_string, exclusions, return_type, maxTries, include.dirs = FALSE ,recursive = FALSE){

  # Retrieve files
  if ( missing(maxTries)) {
    maxTries <- 1
  }

  # Retrieve file names in folder_path
  count <- 0
  repeat {
    # Look for files
    if (missing(file_string)) {
      file <- list.files(path = file.path(folder_path),
                         include.dirs = include.dirs,
                         recursive = recursive)
    } else {
      file <- list.files(path = file.path(folder_path),
                         include.dirs = include.dirs,
                         recursive = recursive,
                         pattern = file_string)
    }
    # Keep looking up to maxTries
    if (count <= maxTries) {

      count <- count + 1
      # Stop looking after maxTries
    } else {
      break
    }
    # Stop looking when files retrieved
    if (!is.null(dim(file) )) {
      break
    }

  }
  # Stop function if no files retrieved
  if (length(file) < 1) {

    stop("No files found. Check folder_path and your connection and increase maxTries if necessary")

  }

  # Exclude unwanted files
  if ( missing(exclusions)) {
    file <- file[!grepl("\\$", file)]
  } else {
    file <- file[!grepl(paste("\\$", exclusions, sep = "|"), file)]
  }

  # Stop function if no files left
  if (length(file) < 1) {

    stop("All files from folder_path have been excluded. Check exclusion criteria and try again.")

  }

  # Retrieve latest file by modification date
  file <- as.data.frame(file, row.names = NULL)
  file[,1] <- as.character(file[,1])

  repeat {

    file <- cbind(file, file.info(file.path(folder_path, file[,1])))

    if (count <= maxTries &  all(is.na(file$ctime))) {

      count <- count + 1

    } else {
      break
    }
  }
  if (count == maxTries &  all(is.na(file$ctime))) {

    stop("File names retrieved but modification date not retrieved. Check your connection and increase maxTries if necessary")

  }

  file <- file[with(file, order(ctime)), ]
  file <- file[NROW(file),]
  file$path <- file.path(folder_path, file$file)
  rownames(file) <- 1

  # Output
  if ( missing(return_type)) {
    return_type <- "path"
  }

  if ( return_type == "all") {

    return(file)

  } else if (return_type == "name") {

    message(paste0("File modification date: ", file$ctime))
    return(as.character(file$file))

  } else if (return_type == "path") {

    message(paste0("File modification date: ", file$ctime))
    return(as.character(file$path))

  }

}


#' Open path using File Explorer or open named file
#'
#' @param path Path to folder or file to open
#'
#' @return Opens a File Explorer window at the given path or file
#'
#' @examples
#' # dir.create("test")
#' # simpleopen("test")
#'
#' @export
simpleopen <- function(path){
  if(!file.exists(path)) stop(paste0("path not found; ", path))
  suppressWarnings(shell(paste0("explorer ", gsub("/", "\\\\", path)), intern = TRUE))
}


#' Title Search a list of files for values in a consistent column
#'
#' @param IDs Id values to look for.
#' @param IDcol Column containing IDs in files.
#' @param files Files to look in
#' @param complete Optional; retain files without matches in list or not. Default is FALSE
#'
#' @return Returns a list with file names and rows of data with listed IDs in the IDCol column
#' @import data.table
#'
#' @examples
#'
#' # simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "testa.rds")
#' # simplewrite(data.frame(a = c(1,2,3, 4), b = c("red", "amber", "green", "blue")), "testb.rds")
#' # files <- list.files(pattern = ".rds")
#' # dt3 <- IDinfilesearch(IDs = c(3,4), IDcol = "a", files = files)
#' # dt4 <- IDinfilesearch(IDs = 4, IDcol = "a", files = files, complete = TRUE)
#'
#' @export
IDinfilesearch <- function(IDs, IDcol, files, complete = FALSE){
  IDs <- as.character(IDs)
  t <- list()
  for (f in files){
    d <- data.table::data.table(simpleimportforce(f))
    col <- names(d)[grepl(IDcol, names(d), ignore.case = T)]
    d[, look:= as.character(d[,get(col)])]
    x <- d[look%in%IDs,][,look := NULL]
    t[[f]] <- x
    message(paste(f, ";",nrow(x), "matching rows"))
    remove(d, f)
  }
  if(complete == FALSE) t <- Filter(f = function(x)nrow(x)>0, x = t)

  return(t)
}

