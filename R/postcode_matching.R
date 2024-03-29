#' Create an Index for an ONS NSPL postcode database
#'
#' Creates an index of a postcode database file using the first letters and first number from within the postcode. This has been designed for use with the NSPL database of UK postcodes and will not work for postcodes which do not start with letters followed by numbers.
#'
#' @param postcode_path path to folder containing multiple postcode csv files or a single csv file. Eg for ONS NSPL this could be either file.path("NSPL_FEB_2019_UK", "Data", "multi_csv") from where the ONS data was extracted if using the multi_csv files or file.path("NSPL_FEB_2019_UK", "Data", "NSPL_FEB_2019_UK.csv") if using the full database file. Using multi_csv files can overcome insufficient memory barriers.
#'
#' @param colname column name within database csv file(s) with postcode for postcode matching (optional). Default is the first column name within the database.
#'
#' @param name name to call the index (optional). Default is "Index".
#'
#' @param exclude string pattern within files to exclude when using a folder as postcode_path (optional).
#'
#' @return Returns a postcode database Index using the first letters from the file names and the first digits of the numbers within the postcodes as a data.frame object. Saves the Index file to the location of the postcode database.
#' @import data.table
#'
#' @author Diane Hatziioanou
#'
#' @keywords postcode, Index
#'
#' @examples
#' # Index <- index_postcodes(postcode_path)
#'
#' # Index_ONS <- index_postcodes("C:/.../NSPL_FEB_2019_UK/Data/multi_csv/)
#'
#' # Index_ONS <- index_postcodes(
#'     # file.path(path, "NSPL_FEB_2019_UK, "Data ,"multi_csv"),
#'     # exclude = "Readme.csv")
#'
#' # Index_file <- index_postcodes(file.path(path, "2020 database, "Reference.csv"),
#'     # colname = "pcd", name = "Reference_index")
#'
#' # Create an index if necessary or read in if already there
#' # if (!file.exists(file.path(NSPL_csvs,"Index.csv"))) {
#' # Index <- index_postcodes(NSPL_csvs)
#' # } else {
#' #   Index <- fread(file.path(NSPL_csvs,"Index.csv"))
#' #  }
#'
#' @export
index_postcodes <- function(postcode_path, colname, name, exclude){
  start_time <- Sys.time()

  if (missing(name)) {
    name <- "Index"
  }

  # Database file path and file name(s)
  if (dir.exists(postcode_path)) { # Directory

    # List files
    Db_files <- list.files(path = file.path(postcode_path),
                           include.dirs = F,
                           recursive = F,
                           pattern = ".csv")
    Db_files <- as.data.frame(Db_files[!grepl("\\$", Db_files)], responseName  = "file", row.names = NULL, optional = F, stringsAsFactors = F)
    data.table::setnames(Db_files, "file")

    data.table::setDT(Db_files)[, full_path := file.path(postcode_path,Db_files$file)]
    Db_files <- Db_files[!file.info(Db_files$full_path)$isdir]
    Db_files <- Db_files[stringr::str_detect(Db_files$file, "^NSPL_") & stringr::str_detect(Db_files$file, ".csv"),]

     # QC csv files to index
    if (missing(exclude)) {
      message(paste("Indexing all csv files within", postcode_path, "as ONE database"))
    } else {

      Db_files <- Db_files[!grepl(paste(exclude, collapse = "|") , Db_files$file),]
      message(paste("Indexing", NROW(Db_files$file), "csv files within", postcode_path, "as ONE database"))

    }
    if (dim(Db_files)[1] == 0) { stop("No csv files found.") }

    # Check database files
    for(i in 1:nrow(Db_files)) {

      db_file <- data.table::fread(file.path(Db_files[i,"full_path"]), header = F, nrows = 1)
      if(exists("db_files")) {

        t <- try(db_files <- data.table::rbindlist(list(data.table::setDT(db_files), data.table::setDT(db_file)),use.names=TRUE, fill=TRUE), silent = TRUE)
        ifelse(inherits(t, "try-error"), stop("Column name mismatch; csv files don't all belong to the same database."),t)

      } else {
        db_files <- db_file
      }
    }

    # Postcode column
    if (missing(colname)) { colname <- as.character(db_files[1,1]) }
    remove(db_files, db_file)
    path <- postcode_path

  } else if (file.exists(postcode_path) && !dir.exists(postcode_path)) { # Single file

    Db_files <- postcode_path
    Db_files <- as.data.frame(Db_files[!grepl("\\$", Db_files)], responseName  = "file", row.names = NULL, optional = F, stringsAsFactors =F)
    data.table::setnames(Db_files, "full_path")
    data.table::setDT(Db_files)[, file := basename(postcode_path)]
    message(paste("Indexing csv file", Db_files$file, "within", postcode_path, "as ONE database"))
    path <- dirname(postcode_path)

    # Postcode column
    if (missing(colname)) {
      colname <- data.table::fread(file.path(Db_files[1,"full_path"]), header = F, select = 1, nrows = 1) %>% as.character()
    }

  } else { stop("Could not find any .csv files at ", postcode_path) }


  # Get file row index positions
  for(i in 1:nrow(Db_files)) {

    # Get postcode letters and first number
    Pc <- data.table::fread(file.path(Db_files[i,"full_path"]), header = T)

    data.table::setDT(Pc)[,block := toupper(gsub(" ", "", Pc[,get(colname)]))]
    Pc[, block := (gsub('([a-zA-Z])([0-9])', '\\1_\\2', block))]
    Pc[, block := paste0(gsub( "_.*$", "",  block), substr(gsub('\\D+','', block), 1,1))]
    Pc[, block := factor(Pc$block)]
    Pc[, row := 1:nrow(Pc)]

    # Get file index rows
    rows <- base::split(1:nrow(Pc), Pc$block)
    index <- sapply(rows, function(rowi) {rowi[which.min(Pc$row[rowi])]})
    index <- data.table::setDT(list(index))[]
    index <- data.table::setnames(index,"start")
    index$stop <-  sapply(rows, function(rowi) {rowi[which.max(Pc$row[rowi])]})
    index[, block := names(rows)]
    remove(rows)

    index[,start := index$start + 1] # count for header row
    index[,stop := index$stop + 1] # count for header row

    index[,full_path := Db_files[i,"full_path"]]
    index[,file := Db_files[i,"file"]]

    # IF MERGED DATASET EXISTS, MERGE
    if (exists("Index") & dim(index)[1] != 0 ) {
      Index <- data.table::rbindlist(list(data.table::setDT(Index), data.table::setDT(index)), use.names= TRUE, fill=TRUE)
    }
    # IF MERGED DATASET DOESN'T EXIST, CREATE IT
    if (!exists("Index")) {
      Index <- index
    }
    remove(index,Pc)
  }
  Index <- Index[!is.na(Index$block),]
  # Save index
  data.table::setcolorder(Index,c("block", "start", "stop", "file", "full_path"))
  data.table::fwrite(x = Index, file.path(path, paste0(name,".csv")), row.names = F, nThread = data.table::getDTthreads())

  # Report
  end_time <- Sys.time()
  message(paste("Run time:",end_time - start_time))
  if(file.exists(file.path(path, paste0(name,".csv")))) message(paste("Index saved at:",file.path(path, paste0(name,".csv"))))
  Index <- as.data.frame(Index)
  return(Index)

}

#' Retrieve ONS postcode data and append to object
#'
#' Retrieves Office for National Statistics (ONS) data by postcode and appends it as added columns to an input file. For information on ONS databases see https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-february-2020
#'
#' @param data data with postcodes for ONS information retrieval
#'
#' @param query_column name of column within dt containing postcodes to retrieve as a string
#'
#' @param Index index object created using index_postcodes()
#'
#' @param desired_columns name of ONS columns to retrieve. Default is "all"; note this excludes 2019 utla and ltla "stp19", "ccg19" geographies which are retrieved from a different database.
#' For UTLA and LTLA "utla", "ltla" can be listed and the 2019 geographies (names and codes) will be retrieved based on the database "laua" from  https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales (2019 version).
#' For the 2020 STP and CCGs  lsoa11, "stp20", "ccg20" can be listed and the 2020 geographies (names and codes) will be retrieved based on the database "lsoa11" from https://geoportal.statistics.gov.uk/datasets/lsoa-2011-to-clinical-commissioning-groups-to-sustainability-and-transformation-partnerships-april-2020-lookup-in-england/data . Herefordshire CCG name is updated as per https://digital.nhs.uk/services/organisation-data-service/change-summary---stp-reconfiguration
#' For PHEC or PHER assign c("laua", "PHEC19CD", "PHEREG19CD") to desired_columns or c("all", "PHEC19CD", "PHEREG19CD").

#' @return Returns the original dt object as a data.frame with added columns of ONS data
#' @import data.table
#'
#' @author Diane Hatziioanou
#'
#' @keywords postcode, ONS, NSPL, Office for National Statistics
#'
#' @examples
#' # Index <- fread(file.path("C:/.../NSPL_FEB_2019_UK/Data/Index.csv"))
#' # data_with_ONS <- match_postcode(data, "Postcode", Index, "all")
#'
#' @export
match_postcode <- function(data, query_column, Index, desired_columns){
  start_time <- Sys.time()
  options(datatable.fread.dec.experiment = FALSE)

  Index <- as.data.frame(Index)
  # Cater for previous verison of Index
  if ("Pc_s" %in% names(Index)) {
    Index <-  dplyr::rename(Index, block = "Pc_s")
  }
  dt <- as.data.frame(data)
  col_order <- names(dt)
  # Check dabatase has desired columns
  db_columns <- as.character(data.table::fread(Index$full_path[1], header = F, nrows = 1, stringsAsFactors = F, fill = T))

  if (missing(desired_columns)) {
    desired_columns <- "all"
  }

  if ("all" %in% desired_columns) {
    desired_columns <- unique(c(db_columns, desired_columns[desired_columns != "all"]))
  } else {
    problem_columns <- desired_columns[!(desired_columns %in% c(db_columns, "utla", "ltla", "stp20", "ccg20", "PHEC19CD", "PHEREG19CD"))]
    if (length(problem_columns) != 0) {
      cat(problem_columns)
      stop(paste("The database doesn't have the desired columns above. Try with ones it has!"))
    }
  }

  # sort by first letters and first number
  data.table::setDT(dt)[,Postcode_match := toupper(gsub(" ", "", dt[,get(query_column)]))]
  dt[, Pc := (gsub('([a-zA-Z])([0-9])', '\\1_\\2', dt$Postcode_match))]
  dt[, Pc := (gsub( "_.*$", "",  dt$Pc))]
  dt[, Pc_N := (substr(gsub('\\D+','', dt[,get(query_column)]), 1,1))]
  dt[, block := paste0(dt$Pc,dt$Pc_N)]
  dt[, block := toupper(dt$block)]
  dt[, block := factor(dt$block)]
  dt[, c("Pc","Pc_N") := NULL]
  data.table::setorder(dt, cols = block, na.last = T)

  # Change any problematic Date columns to character format
  Date_columns <- (colnames(dt)[grepl("Date", sapply(dt,class))])
  if (isTRUE(length(Date_columns) > 0)){
    dt[, (Date_columns) := lapply(.SD, as.character), .SDcols = Date_columns]
  }

  # Retrieve goegraphies from postcodes
  for (i in seq_along(levels(dt$block))) {
    # Subset data by postcode
    temp_dt <- data.table::setDF(dt)[dt$block == levels(dt$block)[i],]

    # Give ONS data to postcodes in database
    if (levels(dt$block)[i] %in% Index$block) {

      # Retrieve ONS postcode geographies
      Pc_ONS <- data.table::fread(input = file.path(Index[Index$block == levels(dt$block)[i],"full_path"]),
                                  header = F,
                                  sep = "auto",
                                  skip = as.numeric(Index[Index$block == levels(dt$block)[i],"start"] - 1),
                                  nrows = as.numeric(Index[Index$block == levels(dt$block)[i],"stop"] - Index[Index$block == levels(dt$block)[i],"start"] + 1))
      # Make postcodes matchable
      Pc_ONS[,Postcode_match := toupper(gsub(" ", "", Pc_ONS$V1))]

      # Add ONS geographies to matched data
      temp_dt <- merge(temp_dt,Pc_ONS, by = "Postcode_match", all.x = T, all.y = F)

    } else {
      # Give NA to postcodes not in database
      setDT(temp_dt)[, colnames(data.table::fread(Index$full_path[1], header = F, nrows = 1, stringsAsFactors = F, fill = T)) := character(.N) ]
    }
    # IF MERGED DATASET EXISTS, MERGE
    if (exists("dt_geocoded", where = -1) & dim(temp_dt)[1] != 0 ) {
      dt_geocoded <- rbindlist(list(dt_geocoded, temp_dt), use.names = T, fill = T)
    }
    # IF MERGED DATASET DOESN'T EXIST, CREATE IT
    if (!exists("dt_geocoded", where = -1)) {
      dt_geocoded <- copy(temp_dt)
    }
    if (!exists("Pc_ONS")) {
      remove(temp_dt)
    } else{
      remove(temp_dt,Pc_ONS)
    }
  }

  data.table::setcolorder(data.table::setDT(dt_geocoded), c(col_order, paste0("V", 1:41)))

  # Add column names
  col_names <- as.character(data.table::fread(Index$full_path[1], header = F, nrows = 1, stringsAsFactors = F, fill = T))
  colnames(dt_geocoded)[((ncol(dt_geocoded) - (NROW(col_names)) - 1)):(ncol(dt_geocoded) - 2)] <- col_names

  # Remove undesired column
  data.table::setDT(dt_geocoded)[, c(db_columns[!(db_columns %in% c(desired_columns, "laua","lsoa11")) &
                                                  db_columns %in% names(dt_geocoded)],
                                     "block", "Postcode_match") := NULL]

  # Convert previous Date columns back to Date format
  if (isTRUE(length(Date_columns) > 0)) {
    dt_geocoded[, (Date_columns) := lapply(.SD, function(x)  as.Date(lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd")))), .SDcols = Date_columns]
  }


  # Add UTLA - LTLA 2020 data if "utla", "ltla in desired_columns
  if (isTRUE(sum(c("utla", "ltla") %in% desired_columns) > 0)) {
    UTLA_LTLA <-  data.table::fread(file.path("https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv"))
    if (sum(c("utla", "ltla") %in% desired_columns) == 2) {
      get_columns <- c( "LTLA19CD", "LTLA19NM", "UTLA19CD", "UTLA19NM")
    } else if ("ltla" %in% desired_columns) {
      get_columns <- c("LTLA19CD", "LTLA19NM")
    } else{
      get_columns <- c("LTLA19CD","UTLA19CD", "UTLA19NM")
    }
    dt_geocoded <- merge(dt_geocoded, data.table::setDF(UTLA_LTLA)[, get_columns], by.x = "laua", by.y = "LTLA19CD", all.x = T, all.y = F)
  }
  # Add CCG - STP 2020 data if "stp20", "ccg20" in desired_columns
  if (isTRUE(sum(c("stp20", "ccg20") %in% desired_columns) > 0)) {
    if (sum(c("stp20", "ccg20") %in% desired_columns) == 2) {
      get_columns <- c("LSOA11CD","CCG20CD", "CCG20NM",  "STP20CD",  "STP20NM")
    } else if ("stp20" %in% desired_columns) {
      get_columns <- c("LSOA11CD","STP20CD",  "STP20NM")
    } else{
      get_columns <- c("LSOA11CD","CCG20CD", "CCG20NM")
    }
    UTLA2011_CCG_STP2020 <-  data.table::fread(file.path("https://opendata.arcgis.com/datasets/1631beea57ff4e9fb90d75f9c764ce26_0.csv"))
    UTLA2011_CCG_STP2020$CCG20NM[grepl("Herefordshire ", UTLA2011_CCG_STP2020$CCG20NM, ignore.case = T)] <- "NHS Herefordshire and Worcestershire CCG"
    dt_geocoded <- merge(dt_geocoded, setDF(UTLA2011_CCG_STP2020)[, get_columns], by.x = c("lsoa11"), by.y = c("LSOA11CD"), all.x = T, all.y = F)
  }

  # PHE Centre data if "PHEC19CD", "PHEREG19CD" in desired_columns
  if (isTRUE(sum(c("PHEC19CD", "PHEREG19CD") %in% desired_columns) > 0)) {
    if (sum(c("PHEC19CD", "PHEREG19CD") %in% desired_columns) == 2) {
      get_columns <- c("LAD19CD","PHEC19CD", "PHEC19CDH", "PHEC19NM", "PHEREG19CD", "PHEREG19CDH", "PHEREG19NM")
    } else if ("PHEC19CD" %in% desired_columns) {
      get_columns <- c("LAD19CD","PHEC19CD", "PHEC19CDH", "PHEC19NM")
    } else{
      get_columns <- c("LAD19CD", "PHEREG19CD", "PHEREG19CDH", "PHEREG19NM")
    }
    LAD19_to_PHE19 <-  data.table::fread(file.path("https://opendata.arcgis.com/datasets/4da177ab2ab34edaba9d2696c3a6da64_0.csv"))
    dt_geocoded <- merge(dt_geocoded, setDF(LAD19_to_PHE19)[, get_columns], by.x = c("laua"), by.y = c("LAD19CD"), all.x = T, all.y = F)
  }

  data.table::setcolorder(data.table::setDT(dt_geocoded), c(col_order, names(dt_geocoded)[!(names(dt_geocoded) %in% col_order)]))

  end_time <- Sys.time()
  message("Run time:")
  message(end_time - start_time)
  message(paste("Postcodes searched:",sum(!is.na((dt_geocoded[,get(query_column)])))))

  new_col <- copy(dt_geocoded[, (ncol(dt) - 1 ):ncol(dt_geocoded)])
  new_col[new_col == ""] <- NA

  not_retrieved <- sum(rowSums(is.na(new_col)) == ncol(new_col))
  found_rows <- sum(rowSums(is.na(new_col)) != ncol(new_col))
  no_postcode <- NROW(dt[is.na(dt[,(query_column)]),])
  message(paste("Rows matched:", found_rows))
  message(paste("Rows with no postcode:", no_postcode))
  message(paste("Rows with postcode not in NSPL:",not_retrieved - no_postcode))
  message(paste("Total rows not matched:",not_retrieved))

  if (isTRUE(length(Date_columns) > 0)) {
    warning("Columns changed to character format and back to Date again: ", list(Date_columns))
  }

  data.table::setDF(dt_geocoded)
  return(dt_geocoded)
  rm(list = ls())
  gc(verbose = F, full = T)
}
