#' Title Create a unique identified based on unique values in columns and a further unique identifier
#' Assign unique IDs to data based either on PII columns or a combination of PII columns and a further unique identifier where available
#'
#' @param x Data to assign unique IDs to
#' @param col PII columns within data to use for assigning unique IDs
#' @param Number Potentially incomplete unique identifier which can be used in combination with PII to assign unique IDs
#' @param name Name to give unique ID column in output
#'
#' @return full dataset with additional ID column returned
#'
#' @examples
#'
#' x <- data.frame(DOB = c("01/01/2001", "02/01/2001", "01/01/2001", "03/01/2001"), name = c("A", "B", "C", "D"), surname = c("A", "B", "A", "D"), ZIP = c("A", "B", "A", "D"), NHS_Number = c(2001, NA, 2001, NA))
#' Number = "NHS_Number"
#' name <- "GroupID"
#' col = c("DOB", "name", "surname", "ZIP")
#' x <- simpleID(x = x, col =  col, name = name)
#' x <- simpleID(x = x, col =  col, Number = Number, name = name)
#' @export
simpleID <- function (x, col= names(x)[grepl("Birth|DOB|Postcode|ZIP|Name", names(x), ignore.case = T)], Number = FALSE, name ="GroupID") {
  x <- copy(data.table::as.data.table(x))
  message("No columns specified, using ", paste0(col, collapse = ", "))
  if (sum(!col %in% names(x)) > 0) {
    stop(paste0("Column not found: ", paste0(col[!col %in%
                                                   names(x)], collapse = ", ")))
  }
  if (Number != FALSE) {
    x[, `:=`(PII, Reduce(function(...) paste0(...), x[,
                                                      mget(col)])), .SD[, mget(col)]]
    x[, `:=`(Number_ID, .GRP), by = get(Number)]
    x[is.na(get(Number)), `:=`(Number_ID, length((max(x$Number_ID,
                                                      na.rm = T) + 1):sum(is.na(x[, get(Number)]), max(x$Number_ID,
                                                                                                       na.rm = T))))]
    g <- igraph::graph_from_data_frame(x[, c("PII", "Number_ID")])
    c <- igraph::clusters(g)$membership
    x[, `:=`((name), c[x$PII])]
    x[, `:=`(PII = NULL, Number_ID = NULL)]
  } else {
    x[, `:=`((name), .GRP), by = list(get(col))]
  }
  x <- as.data.frame(x)
  return(x)
}
