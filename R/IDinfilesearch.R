#' Title Search a list of files for values in a consistent column
#'
#' @param IDs Id values to look for.
#' @param IDcol Column containing IDs in files.
#' @param files Files to look in
#' @param complete Optional; retain files without matches in list or not. Default is FALSE
#'
#' @return Returns a list with file names and rows of data with listed IDs in the IDCol column
#' @importFrom rlang :=
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
