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

#' Title Extract names of tables from sql query
#'
#' @param sqlfile sql query file
#' @param q  query from sql file
#' @param nameonly Optional; list only table/view name. Default is FALSE
#'
#' @return table names from query
#'
#' @examples
#' # sqlfile_tables(q = query1)
#' # sqlfile_tables(sqlfile = "query.sql")
#'
#' @export
sqlfile_tables <- function(sqlfile=NULL, q=NULL, nameonly = FALSE){
  if(is.null(q)) {
    q <- sqlfile_read(sqlfile)
  }
  q <- sql_nocomments(q = as.character(q))
  x <- unlist(strsplit(q, split = "\r\n", fixed = T))
  # remove in line comments
  x <- gsub("\\--.*","",x)
  # remove multiline comments
  com <- strsplit(q, split = "/*", fixed = T)
  com <- unlist(lapply(com, function(x) strsplit(x, split = "*/", fixed = T)))
  com <- com[!(substr(com, 1,1) == "*" & substr(com, nchar(com),nchar(com)) == "*")]
  # find tables
  n <- lapply(x, function(x) grep("FROM",x,ignore.case = T))
  #n <- unlist(n)+1
  y <- x[which(n>0)]
  y2 <- strsplit(y, split = " ", fixed = T)
  n2 <- lapply(y2, function(x) grep("FROM",x,ignore.case = T))
  for(i in 1:length(y2)){
    y2[i] <- y2[[i]][n2[[i]]+1]
  }
  y2 <- unlist(y2)
  if(any(grepl("[", y2,fixed = T))){
    y2[grepl("[", y2,fixed = T)] <- tail(unlist( strsplit(x = gsub(".", " ",y2[grepl("[", y2,fixed = T)], fixed = T), split = " ")),1)
    y2[grepl("[", y2,fixed = T)] <- gsub("]", "",gsub("[", "",y2[grepl("[", y2,fixed = T)],fixed = T),fixed = T)
  }
  return(y2)
}


#' Title Remove comments from SQL files
#'
#' @param sqlfile  sql query file
#' @param q   query from sql file
#'
#' @return query with comments removed
#'
#' @examples
#' # sqlfile_tables(sqlfile = "sqlfile.sql")
#' # sqlfile_tables(q = q1)
#'
#' @export
sql_nocomments <- function(sqlfile=NULL, q=NULL) {
  if(is.null(q)) {
    q <- sqlfile_read(sqlfile)
  }
  x <- unlist(strsplit(q, split = "\r\n", fixed = T))
  # remove in line comments
  x <- gsub("\\--.*","",x)
  # remove multiline comments
  com <- strsplit(q, split = "/*", fixed = T)
  com <- unlist(lapply(com, function(x) strsplit(x, split = "*/", fixed = T)))
  com <- com[!(substr(com, 1,1) == "*" & substr(com, nchar(com),nchar(com)) == "*")]
  com <- glue::glue(paste(com, collapse = "\r\n"))
  return(com)
}
