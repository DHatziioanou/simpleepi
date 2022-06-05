#' Title Open File Explorer at a specified path or open files from R.
#'
#' @param path Path to folder or file to open
#'
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
