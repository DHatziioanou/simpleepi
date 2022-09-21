#' Title Manage duplicates in line list
#'
#' @param x Data
#' @param namecols name columns
#' @param dobcol dob column
#' @param pccol postcode column
#' @param idcols id columns
#' @param pc_format postcode format
#'
#' @return
#'
#' @examples
#' # simplededup()
#'
#' @export
simplededup <- function(x, namecols = c("first_name","surname"), dobcol = "date_of_birth", pccol = "postcode", idcols = c("nhs_number"), pc_format = "lnl"){

  # Format
  x <- copy(x)
  if(any(class(x)== "data.table")) data.table::setDT(x)
  x[ , (dobcol) := simpledates(get(dobcol), char = NA)]
  # Name combinations for each person
  x$temp_n <- name_combos(x = x[,..namecols], namecols =  c("first_name","surname"))
  # Name search

  # DOB search

  # PC search

  # ID search

  # Combine scores+missing

  # Mark dup candidates with overall score

  # Aggregate dup in out1 with score col
  ## Duplicate_notes; combined from v1, v2

  # Return dup as input in out2


}

#' Title Format names from a data.table and make return list of all name combinations per row.
#'
#' @param x data.table of names
#' @param namecols names of column within x with names to process
#'
#' @return list of all name combinations
#'
#' @examples
#'  dt <- data.table(first_name = c("Jane", "Pete"), surname = c("Brown", "McKenzie"))
#'  dt$namelist <- name_combos(x = dt, namecols =  c("first_name","surname"))
#'
#' @import data.table
#' @export
name_combos <- function(x, namecols){
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
