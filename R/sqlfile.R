#' Title Read sql query file
#'
#' @param sqlfile sql query file
#'
#' @return sql file formatted for use in R
#'
#' @examples
#' query <- sqlfile_read("C:/Users/path/query1.sql")
#'
#' @export
sqlfile_read <- function(sqlfile){
  q <- glue::glue(paste(readLines(sqlfile, warn = FALSE, skipNul = TRUE, n = -1L), collapse = "\r\n"))
  return(q)
}


#' Title Use sql query file to query database via odbc
#'
#' @param server Server holidng data
#' @param database  Database to query
#' @param sqlfile sql query file
#' @param name object to give retrieved data
#'
#' @return retrieves data to .GlobalEnv
#'
#' @examples
#' # sqlfile_query(server = "ser.path", database ="W001", sqlfile  = "query.sql", name = "df")
#'
#'
#' @export
sqlfile_query <- function(server, database, sqlfile, name = "df"){
  start <- Sys.time()

  if(!("SQL Server" %in% odbc::odbcListDrivers()$name)) stop("SQL Server driver missing. Install SQLSRV32.DLL then try again")
  q <- sqlfile_read(sqlfile)
  con <- try(odbc::dbConnect(odbc::odbc(),
                             driver="SQL Server",
                             server = server,
                             database = database))
  if(class(con) =="try-error") {
    stop("Login failed. Check you have permission to access this database.")
  } else if(con@info$dbname == database) message("Connected")

  # Run query
  try(assign(name, odbc::dbGetQuery(con, q), envir=as.environment(1)))
  odbc::dbDisconnect(con)

  if(is.null(nrow(get(name,envir=.GlobalEnv))) | nrow(get(name,envir=.GlobalEnv))==0){
    rm(list = name, pos = ".GlobalEnv")
    stop("Error retrieving data. Try again.")
  } else {
    message(paste0("Retrieved ", nrow(get(name, envir = .GlobalEnv)), " rows of data from "), database)
  }
  message(paste("Run time",round(Sys.time()-start, 3), units(Sys.time()-start), sep = " "))
}





