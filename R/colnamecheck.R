#' Title List duplicate column names in 1 or more objects. Disregards case.
#'
#' @param x list of objects
#'
#' @return Returns list with same = duplicates in same object, other = duplicates between objects
#'
#' @examples
#' colnamecheck(df)
#' colnamecheck(list(df1, df2, df3))
#'
#' @export
colnamecheck <- function(x) {
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
