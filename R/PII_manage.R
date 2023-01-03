#' Title Format full names from a data.table and return unique names per row.
#'
#' @param x data.table or data.frame containing with columns containing messy names
#' @param namecols column names containing names (forename(s), middlename(s), surname(s)) to process
#'
#' @return list of unique names
#'
#' @examples
#'  # dt <- data.table(first_name = c("Jane", "Pete"), surname = c("Brown", "McKenzie"))
#'  # dt$namelist <- name_combos(x = dt, namecols =  c("first_name","surname"))
#'
#' @import data.table
#' @export
vectorise_unique <- function(x, namecols){
  # Name combinations for each person
  data.table::setDT(x)
  temp_names <- data.table::copy(x[,..namecols])
  temp_names[ , (namecols) := lapply(.SD, simplewords), .SDcols = namecols]
  names.list <- unlist(apply(temp_names, 1, list), recursive = FALSE)
  names.list <- lapply(names.list, function(y) data.table::tstrsplit(x = y, split = " ",fill =NA))
  names.list <- lapply(names.list, function(y) y <- unique(unlist(y)[!is.na(unlist(y)) & unlist(y) != ""]))
  # Combinations of each name
  for(l in 1:length(names.list)) {
    n <- names.list[[l]]
    for(i in 1:length(n)) {
      if(length(n) > 1){
        if(i == 1){
          y <- paste(n[i],n[-i], collapse=" ")
        } else {
          y <- c(y,paste(n[i],n[-i], collapse=" "))
        }
      } else {
        y <- n
      }

      if(i == length(x)) {
        n <- y
      }
    }
    names.list[[l]] <- n
  }
  return(names.list)
}
