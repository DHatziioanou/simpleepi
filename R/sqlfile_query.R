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
sqlfile_query <- function(server = load_data$CVD19_Server, database ="W126_Covid_OST_Surveillance", sqlfile  = file.path("R", "SQL", "CVD19_episode_var_QAall.sql"), name = "df"){
  start <- Sys.time()
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("package odbc required")
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("package glue required")
  }
  if(!("SQL Server" %in% odbc::odbcListDrivers()$name)) stop("SQL Server driver missing. Install SQLSRV32.DLL then try again")
  q <- glue::glue(paste(readLines(sqlfile), collapse = "\r\n"))
  con <- try(odbc::dbConnect(odbc::odbc(),
                             Driver="SQL Server",
                             Server = server,
                             Database =database,
                             Trusted_Connection="True"))
  if(class(con) =="try-error") {
    stop("Login failed. Check you have permission to access this database.")
  } else if(con@info$dbname == Database) message("Connected")

  # Run query
  try(assign(name, odbc::dbGetQuery(con, q), envir=.GlobalEnv))
  odbc::dbDisconnect(con)

  if(is.null(nrow(get(name,envir=.GlobalEnv))) | nrow(get(name,envir=.GlobalEnv))==0){
    rm(list = name, pos = ".GlobalEnv")
    stop("Error retrieving data. Try again.")
  } else {
    message(paste0("Retrieved ", nrow(get(name, envir = .GlobalEnv)), " rows of data from "), database)
  }
  message(paste("Run time",round(Sys.time()-start, 3), units(Sys.time()-start), sep = " "))
}
