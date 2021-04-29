#' Title Append object data in environment throughout loop
#'
#' @param x data to append during a loop
#' @param name name to call database of appended x
#'
#' @return object in .GlobalEnv containing data from x with new data from itterations of the code appended at the end
#'
#' @examples
#' for (v in c(1,2,3)){
#'   df <- data.frame(a = stats::runif(3), b = c("a", "b", "c"))
#'   df$version <- v
#'   simpleappend(df, name = "DF")
#' }
#'
#' @export
simpleappendenv <- function(x, name= "DF"){
  if(exists(name)) {
    base::assign(x = name, value = data.table::rbindlist(list(get(name),x), use.names = T,fill = T) , envir = .GlobalEnv)
  } else {
    base::assign(x = name, value = x, envir = .GlobalEnv)
  }
}



