#' Import various formats of files into R with one command. If file is not found a brower will open to select the file
#'
#' Quick import of file types Rdata, RDS, rda, csv, txt, xlsx, xls or dta into your R environment.
#'
#' @param file Optional; file to import. If not in your working directory the path needs to be included. If not specified a browser will open to select a file.
#'
#' @param sheet Optional; sheet name or number within workbooks to import. By default the first tab is imported.
#'
#' @param skip Optional; Number of rows from top of the spreadsheet to skip in the import.
#'
#' @param ... Optional; other import arguments from the fread for csv, readxl for xls or xlsx or haven for dta files.
#'
#' @return files imported into R
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords import
#'
#' @examples
#'
#' simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "test")
#' df <- simpleimport("test.csv")
#'
#' simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "test.xlsx")
#' df <- simpleimport("test.xlsx")
#'
#'
#' simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "test.rds")
#' df <- simpleimport("test.rds")
#'
#' @export
simpleimport <- function(file, sheet, skip, ...){
  if (missing(skip)) {skip <- 0}
  if (missing(file)) {
    message("File not found, select a file from the browser")
    file <- file.choose()
  }
  if(!(isTRUE(try(httr::HEAD(file)[2] == 200, silent = T)))){
    if(!(file.exists(file)) ){
      message("File not found, select a file from the browser")
      file <- file.choose()
    }
  }

  if (grepl("csv|txt", tools::file_ext(file), ignore.case = T)) {
    df <- data.table::fread(file, header = T, stringsAsFactors = F, showProgress = T, skip = skip, na.strings= c("NA", "NULL", NULL), encoding = "UTF-8", ...)
  } else if (grepl("xlsx|xlsm", tools::file_ext(file), ignore.case = T)) {
    if (missing(sheet)) {sheet <- 1}
    df <- readxl::read_xlsx(file, sheet = sheet,  col_names  =  T, skip = skip, ...) #col_types = "text",
  } else if (grepl("xls", tools::file_ext(file), ignore.case = T)) {
    if (missing(sheet)) {sheet <- 1}
    df <- readxl::read_xls(file, sheet = sheet,  col_names  =  T, col_types = "text", skip = skip, ...)
  } else if (grepl("dta", tools::file_ext(file), ignore.case = T)) {
    df <- haven::read_dta(file, encoding = NULL, skip = skip, ...)
  } else if (grepl("rda|rdata", tools::file_ext(file), ignore.case = T)){
    load(file, verbose = T)
    ifelse(length(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))]) > 1,
           df <- list(get(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))])),
           df <- get(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))]))
  } else if (grepl("rds", tools::file_ext(file), ignore.case = T)){
    df <- readRDS(file)
  } else {
    stop("unsupported file type")
    invisible(readline(prompt="Unsupported file type, Press [enter] to continue or [Esc] to stop and retry"))
    }
  message(paste0("Imported ", ifelse(grepl("csv|xls|dta|rds", tools::file_ext(file), ignore.case = T), file, paste0(file, " containing ", paste0(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))], collapse = ", ")))))
  return(df)
}


#' Import various formats of files into R with one command.
#'
#' Quick import of file types Rdata, RDS, rda, csv, txt, xlsx, xls or dta into your R environment.
#'
#' @param file Optional; file to import. If not in your working directory the path needs to be included. If not specified a browser will open to select a file.
#'
#' @param sheet Optional; sheet name or number within workbooks to import. By default the first tab is imported.
#'
#' @param skip Optional; Number of rows from top of the spreadsheet to skip in the import.
#'
#' @param ... Optional; other import arguments from the fread for csv, readxl for xls or xlsx or haven for dta files.
#'
#' @return files imported into R
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords import
#'
#' @examples
#'
#' simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "test")
#' df <- simpleimportforce("test.csv")
#'
#' simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "test.xlsx")
#' df <- simpleimportforce("test.xlsx")
#'
#'
#' simplewrite(data.frame(a = c(1,2,3), b = c("red", "amber", "green")), "test.rds")
#' df <- simpleimportforce("test.rds")
#'
#'
#' @export
simpleimportforce <- function(file, sheet, skip, ...){
  if (missing(skip)) {skip <- 0}
  if (missing(file)) {
    stop("File not found, select a file from the browser")
  }
  if(!(isTRUE(try(httr::HEAD(file)[2] == 200, silent = T)))){
    if(!(file.exists(file)) ){
      stop("File not found, select a file from the browser")
    }
  }

  if (grepl("csv|txt", tools::file_ext(file), ignore.case = T)) {
    df <- data.table::fread(file, header = T, stringsAsFactors = F, showProgress = T, skip = skip, na.strings= c("NA", "NULL", NULL), encoding = "UTF-8", ...)
  } else if (grepl("xlsx|xlsm", tools::file_ext(file), ignore.case = T)) {
    if (missing(sheet)) {sheet <- 1}
    df <- readxl::read_xlsx(file, sheet = sheet,  col_names  =  T, skip = skip, ...) #col_types = "text",
  } else if (grepl("xls", tools::file_ext(file), ignore.case = T)) {
    if (missing(sheet)) {sheet <- 1}
    df <- readxl::read_xls(file, sheet = sheet,  col_names  =  T, col_types = "text", skip = skip, ...)
  } else if (grepl("dta", tools::file_ext(file), ignore.case = T)) {
    df <- haven::read_dta(file, encoding = NULL, skip = skip, ...)
  } else if (grepl("rda|rdata", tools::file_ext(file), ignore.case = T)){
    load(file, verbose = T)
    ifelse(length(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))]) > 1,
           df <- list(get(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))])),
           df <- get(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))]))
  } else if (grepl("rds", tools::file_ext(file), ignore.case = T)){
    df <- readRDS(file)
  } else {
    stop("unsupported file type")
    invisible(readline(prompt="Unsupported file type, Press [enter] to continue or [Esc] to stop and retry"))
  }
  message(paste0("Imported ", ifelse(grepl("csv|xls|dta|rds", tools::file_ext(file), ignore.case = T), file, paste0(file, " containing ", paste0(ls()[!(ls() %in% c("sheet", "file", "skip", "df"))], collapse = ", ")))))
  return(df)
}
