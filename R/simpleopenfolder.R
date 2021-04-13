#' Title Open File Explorer at a specified path. Only tested for Windows
#'
#' @param path Path to open in a File Explorer
#'
#'
#' @return Opens a File Explorer window at the given path
#'
#' @examples
#' dir.create("test")
#' simpleopenfolder("test")
#'
#' @export
simpleopenfolder <- function(path){
  suppressWarnings(shell(paste0("explorer ", gsub("/", "\\\\", path)), intern = TRUE))
}
