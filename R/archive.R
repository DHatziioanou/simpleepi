#' Archives files from a folder based on modification date.
#' All files within folder are processed based on modification date.
#'
#' @param from Path to folder containing files to archive.
#'
#' @param to Folder name to archive files to. Default is Archive
#'
#' @param date File modification date from which files are to retained. Files prior to this date are archived. Default is Sys.Date().
#'
#' @param dir Optional logical argument; also archive folders or not. Default is FALSE.
#'
#' @param string Optional string pattern in files to archive.
#'
#' @param exclude Optional string pattern of files to exclude from archiving
#'
#' @param keep Optional retain archived files in original location and copy to archive folder. Default is FALSE.
#'
#' @return Moves older files based on date to an archive subfolder
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords archive
#'
#' @examples
#'
#' # Move all files excluding folders to an Archive subfolder
#' archive("C:..../analysis")
#'
#' # Move all files excluding folders to a different folder
#' archive(from = "C:..../analysis", to = "C:.../backup/outputs")
#'
#' # Copy all files and folders with the "output_version_x" in the file name which have not been modified in the past week to a different folder
#' archive(from = "C:..../analysis", to = "C:.../backup/outputs", date = Sys.Date() -7,
#'         string = "output_version_x", keep = T, dir = T)
#'
#'
#' @export
archive <- function(from, to = "Archive", date = Sys.Date(), dir = FALSE, string = "", exclude = NA, keep = FALSE) {

  # Destination type
  destination <- ifelse(length(stringr::str_split(to, pattern = "/")[[1]])>1, "path", "subfolder")
  if(length(from)>1) stop("Argument -from- should be a single folder")

  # Archive folder present
  if(destination == "path"){
    if (!(dir.exists(to))) {
     dir.create(file.path(to))
    }
  } else if (destination == "subfolder" & !(dir.exists(file.path(from, to)))) {
     dir.create(file.path(from, to))
  }

  # Files to archive
  files <- list.files(path = from, recursive = F, pattern = string)
  files <- files[!grepl(paste0(exclude, collapse = "|"), files)]
  files <- as.data.frame(files)
  files$files <- as.character(files$files)
  files$Modified <- file.info(file.path(from, files[,1]))$ctime
  files$paths  <- file.path(from, files$file)
  if(dir == F){
    files <- files[files$Modified < date &
                   !(files$paths %in% list.dirs(from, recursive = F)),]
  } else {
    files <- files[files$Modified < date,]
  }
  if(nrow(files)==0) {
    message("No files to archive")
    } else {
  # Archive files
  ifelse(keep == T,
   # Copy files
   ifelse(destination == "path",
          file.copy(from = files$paths, to = folder, overwrite = TRUE, recursive = TRUE,
                    copy.mode = TRUE, copy.date = TRUE),
          file.copy(from = files$paths, to = file.path(path,folder), overwrite = TRUE, recursive = TRUE,
                    copy.mode = TRUE, copy.date = TRUE)),
   # Move files
   ifelse(destination == "path",
         file.rename(from = files$paths, to = file.path(to, files$files)),
         file.rename(from = files$paths, to = file.path(from, to, files$files))))
    }
}
