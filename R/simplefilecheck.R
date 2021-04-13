#' Title Check if two files are present and of the same size and same creation and modification time
#'
#' @param x file 1 to check
#' @param y file 2 to check
#'
#' @return  TRUE if both files exist, FALSE if the files are not identical
#'
#' @examples
#'
#' simplewrite(list("test"), "file1.csv")
#' simplewrite(list("test"), "file2.csv")
#' simplefilecheck("file1.csv", "file2.csv")
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
