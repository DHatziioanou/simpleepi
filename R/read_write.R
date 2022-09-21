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

#' Write data to file in various formats with one command
#'
#' Write data into csv, xlsx, xls, rds, rdata or dta files
#'
#' @param data object with data to write. For rdata can be a single object or a list of objects.
#'
#' @param file file name and path to write to. If missing the object name will be used. No extension is needed, this will be added.
#'
#' @param type file type to save data as as a string. If not defined the file name extension will be used or "csv" if no extention given. Options are csv, xlsx, xls, rdata, rdsd, dta.
#'
#' @param sheet Optional; for xlsx and xls the sheet name to write data to.
#'
#' @return writes data to file
#'
#'
#' @author Diane Hatziioanou
#'
#' @keywords write, save
#'
#' @examples
#'
#' simplewrite(data, "C:..../file.csv")
#'
#' simplewrite(data, "C:..../file.xlsx", sheet = "Data")
#'
#' simplewrite(data, "C:..../file.xls", sheet = 4)
#'
#' @export
simplewrite <- function(data, file, type, sheet){
  if (missing(data)) stop("No data to write")
  # File name
  if (missing(file)) {file <- paste0(deparse(substitute(data)), format(Sys.Date(), "%Y%m%d"))}
  filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
  if (missing(type)) {ifelse(tools::file_ext(file) !="",  type <- tolower(tools::file_ext(file)), type <- "csv")}
  ifelse(dirname(file)==".", file <- filename, file <- file.path(dirname(file),filename))
  if (type == "csv") {
    data.table::fwrite(data, paste0(file, ".csv"), row.names = F, col.names = T, append = F)
  } else if (type == "xlsx") {
    if (missing(sheet)) sheet <- paste0(format(Sys.Date(), "%Y%m%d"), " data")
    style <- openxlsx::createStyle(fontSize = 11, halign = "center", border = "TopBottomLeftRight", borderStyle = "thick", fontColour = "black", wrapText = T, textDecoration = "bold")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, data, startCol = 1, startRow = 1, borders = "surrounding", borderStyle = "thick", headerStyle = style)
    openxlsx::setColWidths(wb, sheet, cols = 1:ncol(data), widths = "auto")
    openxlsx::addFilter(wb, sheet, row = 1, cols = 1:ncol(data))
    # SAVE WORKBOOK
    openxlsx::saveWorkbook(wb, paste0(file, ".xlsx"), overwrite = T)
  } else if (type == "xlsx2"| type == "xls") {
    if (missing(sheet)) sheet <- paste0(format(Sys.Date(), "%Y%m%d"), " data")
    readxl::write.xlsx(data, paste0(file, ".xls"), sheetName = sheet, col.names = T, row.names = F, append = F)
  } else if (type == "dta") {
    haven::write_dta(data,  paste0(file, ".dta"), version = 14)
  } else if (grepl("rda|rdata", type, ignore.case = T)) {
    save(data, file = paste0(file, ".rdata"))
  } else if (grepl("rds", type, ignore.case = T)) {
    saveRDS(object = data, file = paste0(file, ".rds"), ascii = FALSE, compress	= "gzip", version = "3")
  }
}

