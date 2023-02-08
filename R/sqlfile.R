#' Title Read sql query file
#'
#' @param sqlfile sql query file
#'
#' @return sql file formatted for use in R
#'
#' @examples
#' # query <- sqlfile_read("C:/Users/path/query1.sql")
#'
#' @export
sqlfile_read <- function(sqlfile){
  q <- glue::glue(paste(readLines(sqlfile, warn = FALSE, skipNul = TRUE, n = -1L), collapse = "\r\n"))
  return(q)
}


#' Title Use sql query file to query database via odbc
#'
#' @param server Server holding data
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





#' Title Import dataset from SQL server into R
#'
#' @param name name to call retrieved data in environment
#' @param server Server holding data
#' @param database Database to query
#' @param schema_name dataset schema eg "dbo"
#' @param dataset table or view dataset name
#' @param columns column names
#' @param top Optional number of top rows to retrieve
#'
#' @return Dataset contents as defined by arguments from SQL server
#' @import DBI
#' @import data.table
#'
#' @examples
#' # sqldb_retrieve(name ="dt", server="x", database="W001", dataset ="x_linelist_v1")
#' # sqldb_retrieve(name ="dt", server="x", database="W001", dataset ="x_linelist_v1",
#'                     # columns=c("id", "value"), top =100)
#'
#' @export
sqldb_retrieve <- function(name ="dt", server, database, schema_name = NULL, dataset, columns=FALSE, top = FALSE){

  start <- Sys.time()
  # connect
  if(!("SQL Server" %in% odbc::odbcListDrivers()$name)) stop("SQL Server driver missing. Install SQLSRV32.DLL then try again")
  con <- DBI::dbConnect(odbc::odbc(),
                        driver = "SQL Server",
                        server = server,
                        database = database,
                        trusted_connection = "true",
                        Encrypt="true")

  if(class(con) =="try-error") {
    stop("Login failed. Check you have permission to access this database.")
  } else if(con@info$dbname == database) message("Connected")

  # dataset retrieve full path
  if(is.null(schema_name)){
    tables <- DBI::dbGetQuery(con, glue::glue("SELECT OBJECT_SCHEMA_NAME(v.object_id) schema_name, v.name FROM sys.tables as v WHERE name LIKE '{dataset}';"))
    tables <- tables[tables$name == dataset,][1, ]
    if(nrow(tables) == 0) stop("dataset not found")
    schema_name <- tables$schema_name
  }
  # dataset retrieve columns
  cols <- try(odbc::dbGetQuery(con, glue::glue("SELECT ORDINAL_POSITION, COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH
                                        FROM INFORMATION_SCHEMA.COLUMNS
                                        WHERE TABLE_NAME='{dataset}'")))
  # dataset manage columns
  data.table::setDT(cols)[, group_order := seq_len(.N), by = list(DATA_TYPE,CHARACTER_MAXIMUM_LENGTH)]
  cols[,order:= data.table::fifelse(is.na(CHARACTER_MAXIMUM_LENGTH) & DATA_TYPE == "bigint", 1000,
                                    data.table::fifelse(is.na(CHARACTER_MAXIMUM_LENGTH) & DATA_TYPE == "int", 2000,
                                                        data.table::fifelse(is.na(CHARACTER_MAXIMUM_LENGTH) & DATA_TYPE == "date", 3000,
                                                                            data.table::fifelse(CHARACTER_MAXIMUM_LENGTH == -1 & DATA_TYPE == "varchar", 8000,
                                                                                                data.table::fifelse(CHARACTER_MAXIMUM_LENGTH == -1 & DATA_TYPE == "nvarchar", 9000, 5000)))))]
  cols <- cols[order(order, CHARACTER_MAXIMUM_LENGTH, group_order, decreasing = F),]
  cols[,COLUMN_NAME := paste0("[",COLUMN_NAME, "]")]
  # columns to retrieve
  if(columns[1] == FALSE){columns <- cols$COLUMN_NAME}
  columns <- unlist(lapply(columns, function(x) {
    if(substr(x, 1,1) != "[" & substr(x, nchar(x),nchar(x)) != "]" ){x <- paste0("[",x, "]")}else{x}
  }))

  if(any(!(columns %in% cols$COLUMN_NAME))) stop(paste("Columns not in", dataset,";", paste0(columns[!(columns %in% cols$COLUMN_NAME)], collapse = ",")))
  cols <- cols[COLUMN_NAME %in% columns,]
  query_cols <- paste(cols$COLUMN_NAME, collapse = ", ")

  # Query full table
  if(top == FALSE){q <- glue::glue("SELECT {query_cols} FROM {database}.{schema_name}.{dataset}")
  } else {q <- glue::glue("SELECT TOP ({top}) {query_cols} FROM {database}.{schema_name}.{dataset}")}

  dt <- try(assign(name, DBI::dbGetQuery(con, q), envir=.GlobalEnv))
  QC_nrows <- try(DBI::dbGetQuery(con, glue("SELECT COUNT(*) from {database}.{schema_name}.{dataset}")))
  if(top != FALSE){
    if(QC_nrows>top) QC_nrows<-top
  }
  DBI::dbDisconnect(con)
  data.table::setcolorder(dt, neworder = gsub("]", "", gsub("[", "", columns, fixed = T)))

  # QC
  if(is.null(nrow(get(name,envir=.GlobalEnv))) | nrow(get(name,envir=.GlobalEnv))!=QC_nrows){
    rm(list = name, pos = ".GlobalEnv")
    invisible(readline(prompt="Error retrieving data, Press [enter] to continue or [Esc] to stop and retry"))
    stop("Error retrieving data. Try again.")
  }
  message(paste0("Retrieved ", nrow(get(name, envir = .GlobalEnv)), " rows of data from "), dataset)
  message(paste("Run time",round(Sys.time()-start, 3), units(Sys.time()-start), sep = " "))

}


vectorise_comma_separated <- function(s) {
  out <- unlist(lapply(strsplit(s, ","), function(x) gsub("\n","",x)))
  return(out)
}
