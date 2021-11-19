#' Title Create a unique identified based on unique values in columns and a further unique identifier
#' Assign unique IDs to data based either on PII columns or a combination of PII columns and a further unique identifier where available
#'
#' @param x Data to assign unique IDs to
#' @param col PII columns within data to use for assigning unique IDs
#' @param Number Potentially incomplete unique identifier which can be used in combination with PII to assign unique IDs
#' @param name Name to give unique ID column in output
#'
#' @return full dataset with additional ID column returned
#' @importFrom rlang :=
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
  y <- data.table::copy(data.table::as.data.table(x))
  message("Using columns ", paste0(col, collapse = ", "))
  if (sum(!col %in% names(y)) > 0) {
    stop(paste0("Column not found: ", paste0(col[!col %in% names(y)], collapse = ", ")))
  }
  if (Number != FALSE) {
    y[, `:=`(PII, Reduce(function(...) paste0(...), y[, mget(col)])), data.table::.SD[, mget(col)]]
    y[, `:=`(Number_ID, .GRP), by = get(Number)]
    y[is.na(get(Number)), `:=`(Number_ID, length((max(y$Number_ID,
                                                      na.rm = T) + 1):sum(is.na(y[, get(Number)]), max(y$Number_ID,
                                                                                                       na.rm = T))))]
    g <- igraph::graph_from_data_frame(y[, c("PII", "Number_ID")])
    c <- igraph::clusters(g)$membership
    y[, `:=`((name), c[y$PII])]
    y[, `:=`(PII = NULL, Number_ID = NULL)]
  } else {
    y[, `:=`((name), .GRP), by = list(get(col))]
  }
  y <- as.data.frame(y)
  return(y)
}
