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
