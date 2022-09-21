#' Title Column names list duplicates in 1 or more objects. Disregards case.
#'
#' @param x list of objects
#'
#' @return Returns list with same = duplicates in same object, other = duplicates between objects
#'
#' @examples
#' # colnamecheck(df)
#' # colnamecheck(list(df1, df2, df3))
#'
#' @export
col_namecompare <- function(x) {
  n <- lapply(x, names)
  nl <- lapply(n, tolower)

  # duplicates in same and other file
  ds <- lapply(nl, duplicated)
  do <- duplicated(unlist(nl))

  dup <- list(same = NA, other = NA)
  # duplicate names same ob
  if(sum(unlist(ds))>0) {
    p1 <- Map(`[`, n, ds)
    dup[["same"]] <- p1
  }
  # duplicate names other ob
  if(sum(do)>0) {
    unlist(nl)[do]
    p2 <- lapply(n, function(x) x[grepl(paste0(unlist(nl)[do], collapse = "|"), ignore.case = T, x)])
    dup[["other"]] <- p2
  }
  return(dup)
}


#' Title Column name search for string patterns
#'
#' @param dt  object such as dataframe, datatable, list
#' @param strings one or more strings to search for
#' @param out return column names or positions
#'
#' @return returns column names or positions
#'
#' @examples
#' # col_nameregex(dt,"dob", out = "names")
#'
#' @export
col_nameregex <- function(dt, strings, out = "names"){
  if(length(strings)>1) strings <-  paste0(strings, collapse = "|")
  x <- names(dt)[grepl(strings, ignore.case = T, names(dt))]
  if(out == "names") {
    return(x)
  } else {
    pos <- unlist(lapply(x,function(x) which( colnames(dt)==x)))
    return(pos)
  }
}
