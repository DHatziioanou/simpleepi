#' Fuzzy match a custom line list to a dabatase by Suname, first name and DOB
#'
#' Matches custom line list persons by full name and DOB to a database, assigns scores and creates warnings based on the match quality and returns the original line list with all the columns of the database and database information where there is a match to the custom line list
#'
#'
#' @param lla Input Line-list list containing the items ob, sn, fn, dob, pc, geo
#' @param ob Line-list object to search. Rows with no surname or first name will be excluded
#' @param sn surname column within the Line-list object as a string
#' @param fn first name column within the Line-list object as a string
#' @param dob date of birth column within the Line-list object as a string
#' @param pc postcode column within the Line-list object as a string
#' @param geo Geography where most individuals can be found. This string must be present in a column of dba.
#'
#' @param dba Input database list containing the items ob, sn, fn, dob, pc, geo
#' @param ob database object to search. All columns of this database will be appended to the input line list
#' @param sn surname column within the database object as a string
#' @param fn first name column within the database object as a string
#' @param dob date of birth column within the database object as a string
#' @param pc postcode column within the database object as a string
#' @param geo Geography column containing the location in string format from lla geo where most individuals can be found.
#'
#'
#' @param stringency Optional; stringency required stringency of matching; one of high, medium or low. Default is high.
#' @param pc_format postcode format where l = letters and n = numbers. Default is the UK format "lnl"
#' @param case_column Column within the custom line list with one unique case number per row
#' @param parallel Optional; process in parallel. Default is TRUE
#'
#'
#' @return Returns the original input object as a data.frame with added columns of the input databse where there is a match base on full name and DOB
#'
#'
#' @author Diane Hatziioanou
#'
#' @examples
#' # Prepare database to include the required columns and anythign else to be appended to the custom line list
#' db <- db[,c("patient_surname", "patient_forename", "patient_date_of_birth", "Postcode", "specimen_date", "Result")]
#' LL <- LL[,c("patient_surname", "patient_forename", "patient_date_of_birth", "Postcode", "specimen_date, "ETHNICITY")]
#'
#' # Create lists of the objects and columns to fuzzy match
#' lla <- list(ob = custom_LL, sn = "SURNAME", fn = "FORENAMES", dob = "BIRTHDATE", pc = "Postcode")
#' dba <- list(ob = db, sn = "patient_surname", fn = "patient_forename", dob = "patient_date_of_birth", pc = "Postcode")
#'
#' # Create a new line list with the database information appended
#' Positives <- fuzzy_match(lla, dba, stringency = F, pc_format = "lnl", case_column = "ID")
#'
#' @export
fuzzy_match <- function(lla, dba, stringency, pc_format, case_column, parallel){
  start_time <- Sys.time()

  # Stringency
  if (base::missing(stringency)) {
    stringency <- "high"
  }
  # Postcode pattern
  if (missing(pc_format)) {
    pc_format <- "lnl"
  }
  l <- "[A-Za-z]"
  n <- "[0-9]"
  pattern <- paste0(get(substr(pc_format,1,1)),".*")
  if(length(pattern >1)){
    for (letter in 1:(nchar(pc_format)-1)){
      pattern <- paste0(pattern, get(substr(pc_format,letter+1,letter+1)), collapse = "*")
    }
  }
  # Parallel processing
  if(missing(parallel)){
    parallel <- T
  }

  # Format to data.table for speed
    dbaob <- copy(as.data.table(dba$ob))
    llaob <- copy(as.data.table(lla$ob))

  if (!(missing(case_column))) {
    # If duplicate case numbers stop
    if(sum(duplicated(llaob[,get(case_column)])) >0 ){
      stop(paste0("Duplicates found in ", case_column, ". Remove duplicates or omit the case_number argument to assign new case numbers"))
  }}

    # Data.table
    dbaob <- as.data.table(dba$ob)
    llaob <- as.data.table(lla$ob)
    # Format strings
    dbaob[ , c("db_Surname", "db_Name", "db_Postcode") := lapply(.SD, simplewords), .SDcols = c(dba$sn, dba$fn, dba$pc)]
    llaob[ , c("ll_Surname", "ll_Name", "ll_Postcode") := lapply(.SD, simplewords), .SDcols = c(lla$sn, lla$fn, lla$pc)]
        # Don't search if name completely missing
        llaob <- llaob[!(is.na(ll_Surname)) & !(is.na(ll_Name))]
        dbaob <- dbaob[!(is.na(db_Surname)) & !(is.na(db_Name))]
    dbaob[, db_surname_firstname := paste(db_Surname, db_Name), by = 1:nrow(dbaob)]
    llaob[, ll_surname_firstname := paste(ll_Surname, ll_Name), by = 1:nrow(llaob)]
    setkey(llaob, "ll_surname_firstname")
    # Only search postcodes in national postcode format
    dbaob[!(grepl(pattern, db_Postcode)), db_Postcode := NA]
    llaob[!(grepl(pattern, ll_Postcode)), ll_Postcode := NA]
    # Format DOB
    dbaob[,(dba$dob) := simpledates(dba$ob[, get(dba$dob)]), by = 1:nrow(dbaob)]
    llaob[,(lla$dob) := simpledates(lla$ob[, get(lla$dob)]), by = 1:nrow(llaob)]
    #
    # if(suppressWarnings(sum(!(is.na(lubridate::parse_date_time(dbaob[, get(dba$dob)], orders = c("dmy", "mdy", "ymd")))))) == 0) {
    #   dbaob[, (dba$dob) := as.numeric(dba$ob[, get(dba$dob)])]
    #   dbaob[, (dba$dob) := as.Date(dba$ob[, get(dba$dob)], origin = "1899-12-30")]
    # }
    # dbaob[, db_DOB := as.Date(lubridate::parse_date_time(dbaob[, get(dba$dob)], orders = c("dmy", "mdy", "ymd")))]
    # if(suppressWarnings(sum(!(is.na(lubridate::parse_date_time( llaob[, get(lla$dob)], orders = c("dmy", "mdy", "ymd")))))) == 0) {
    #   llaob[, (lla$dob) := as.numeric(llaob[, get(lla$dob)])]
    #   llaob[, (lla$dob) := as.Date(llaob[, get(lla$dob)], origin = "1899-12-30")]
    # }
    # llaob[, ll_DOB := as.Date(lubridate::parse_date_time( llaob[, get(lla$dob)], orders = c("dmy", "mdy", "ymd")))]

     # Reserve database outside of geography
     if (isTRUE(!is.na(dba$geo))) {
     dbaob_reserve <- copy(dbaob[!(get(dba$geo) %in% lla$geo) ])
     dbaob <- dbaob[(get(dba$geo) %in% lla$geo) ]
     if(nrow(dbaob_reserve)>0){
       reserve_search <- "yes"
     }
     }
     if(!(exists("reserve_search"))){
       reserve_search <- "no"
     }


if(parallel == F){

  # Search
  for (i in 1:nrow(llaob)) {
   par_list <- retrieve_data(i, case_column, llaob, dbaob)
   # Collect results
   if (exists("z")) {
     z <- rbindlist(list(z,par_list[[1]]), fill = T, use.names = T)
   } else {
     z <- copy(par_list[[1]])
   }
  }

} else {
    # Cores
    numCores <- detectCores()
    # Open cluster
    cluster <- makeCluster(numCores[1]-1, outfile = "")
    registerDoParallel(cl = cluster)
    # Search
    par_all <- foreach (i=1:nrow(llaob), .packages = c("data.table"), .verbose = T) %dopar% {
    par_list <- retrieve_data(i, case_column, llaob, dbaob)
    }
    # Close cluster
    stopCluster(cluster)

    # Collect results
    z <- rbindlist(rvest::pluck(par_all, 1), fill = T, use.names = T)
    unmatched <- unlist(rvest::pluck(par_all, 2), use.names=FALSE)
    unmatched <- unmatched[!(unmatched %in% NA)]
    z <- z[!(is.na(db_surname_firstname))]
  }

# Checkpoint results found?
if(exists("z") == F) stop("No matches")

# Add WARNINGS for cases needing checked
# z[, Fuzzy_match_RAG := data.table::fcase(
#   db_surname_firstname==ll_surname_firstname & ll_DOB==db_DOB, "GREEN",
#   !(db_surname_firstname==ll_surname_firstname & ll_DOB==db_DOB) & (Fuz_score >= 2 | Name_score == 1), "AMBER")]
# z[is.na(Fuzzy_match_RAG),] <- "RED"

z[, Case_ID_Warning := ""]
z[, Case_ID_Warning := ifelse(db_surname_firstname==ll_surname_firstname,
                              'name match',
                              'WARNING! name difference'),
  by = 1:nrow(z)]
z[,Case_ID_Warning := ifelse(db_surname_firstname== "" | is.na(db_surname_firstname),
                             'not found',
                             Case_ID_Warning),
  by = 1:nrow(z)]
# summary.factor(z$Case_ID_Warning)

# Add DOB match info


#   DOB check
z[,Case_ID_Warning := ifelse(!(grepl("no DOB", Case_ID_Warning)) & ll_DOB==db_DOB,
  paste0(Case_ID_Warning,' DOB match'),
  paste0("WARNING! ", Case_ID_Warning,' DOB difference')),
  by = 1:nrow(z)]

z[,Case_ID_Warning := ifelse(
  Case_ID_Warning=="name match DOB match",
  'Perfect match',
  Case_ID_Warning),
  by = 1:nrow(z)]
# Tidy warnings
z[,Case_ID_Warning:= gsub("no DOB DOB difference", "no DOB", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING not found no DOB", "not found", Case_ID_Warning)]
#z[,Case_ID_Warning:= gsub(" WARNING WARNING! name difference no DOB", "WARNING! name difference no DOB", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING! WARNING! name difference no DOB", "WARNING! name difference no DOB", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING! name difference DOB match", "WARNING! name difference", Case_ID_Warning)]
z[,Case_ID_Warning:= gsub("WARNING! WARNING! name difference DOB difference", "WARNING! name and DOB difference", Case_ID_Warning)]
#summary.factor(z$Case_ID_Warning)

# Add RAG rating
z[, Fuzzy_match_RAG := ""]
z[, Fuzzy_match_RAG := ifelse(grepl("Perfect match", Case_ID_Warning),
                             "GREEN",
                             ifelse(grepl("WARNING! name match DOB difference|WARNING! name match no DOB", Case_ID_Warning),
                                    "AMBER",
                                    ifelse(is.na(Case_ID_Warning) | Case_ID_Warning == "not found",
                                           "Not found",
                                           "RED"))),
  by = 1:nrow(z)]
z[, Fuzzy_match_RAG := factor(Fuzzy_match_RAG, levels = c("GREEN", "AMBER", "RED"))]

  # Remove very low score data
  removal_cols <- c(c(names(dba$ob)[!(names(dba$ob) %in% c(dba$sn, dba$fn, dba$dob, dba$pc))]), "db_Surname", "db_Name", "db_surname_firstname", "db_DOB", "db_Postcode", "Matched_surname_firstname", "Fuz_score", "Name_score",	"DOB_score",	"PC_score", "Fuzzy_match_RAG",	"Case_ID_Warning")
if (stringency == "low") {
  z[Name_score < 0.5, which(names(z) %in% removal_cols) := NA]
  z[Fuz_score < 0.8, which(names(z) %in% removal_cols) := NA]
} else if (stringency == "medium") {
  z[Name_score < 0.72, which(names(z) %in% removal_cols) := NA]
  z[DOB_score < 0.7 & !is.na(DOB_score), which(names(z) %in% removal_cols) := NA]
} else if(stringency == "high"){
  z[Name_score < 0.75, which(names(z) %in% removal_cols) := NA]
  z[DOB_score < 0.8 & !is.na(DOB_score), which(names(z) %in% removal_cols) := NA]
  z[Fuz_score < 0.9, which(names(z) %in% removal_cols) := NA]
} else if(stringency == "top"){
  z[Name_score < 0.8, which(names(z) %in% removal_cols) := NA]
  z[DOB_score < 0.8 & !is.na(DOB_score), which(names(z) %in% removal_cols) := NA]
  z[Fuz_score < 2.0, which(names(z) %in% removal_cols) := NA]
  }
# remove empty which have been matched
cases_matched <- copy(unique(z[!is.na(z$db_surname_firstname),get(case_column)]))
z <- z[!(is.na(z$db_surname_firstname) & (z[,get(case_column)] %in% cases_matched))]
z <- unique(z)
z <- z[with(z, order(get(case_column), Fuz_score, decreasing = F)), ]

# Order columns and rows
setDT(z)
setcolorder(z, c(case_column,"Fuzzy_match_RAG", "Case_ID_Warning", "ll_surname_firstname", "Matched_surname_firstname", "db_surname_firstname", "ll_DOB", "db_DOB", "ll_Postcode", "db_Postcode", "Fuz_score", "Name_score", "DOB_score", "PC_score", "db_Surname", "db_Name"))
setnames(z, c("ll_surname_firstname", "db_surname_firstname", "Matched_surname_firstname"), c("ll_surname_firstname", "db_surname_firstname", "Matched_surname_firstname"), skip_absent=T)

# Return data.frame
z <- as.data.frame(z)
end_time <- Sys.time()
message("Run time:")
message(end_time - start_time)
return(z)
cat(" Search completed")
}

# Fuzzy search function based on names and DOB
#' Title
#'
#' @param dbaob  database to search against
#' @param sn_fun full name to search with all combinations of all names and DOB
#'
#' @return
#'
#' @examples
#'   # Retrieve db name matches
#'   matches  <- fuz_search(dbaob, sn_fun)
#' @export
fuz_search <- function(dbaob, sn_fun){
  x <- as.character("")
  x <- lapply(sn_fun, function(w) {agrep(w, dbaob$db_surname_firstname, ignore.case=F, max.distance = 0.2, value=TRUE, useBytes = FALSE, costs = list(ins=2, del=2, sub=3))}) # 0.3 for most comprehensive
  # Give sn_fun name used to each list df by level
  for (r in seq_along(x)) {
    x[[r]] <- suppressWarnings(copy(data.table("db_surname_firstname" = x[[r]], "Matched_surname_firstname" = sn_fun[r])))
  }
  # Collect list results
  x <- rbindlist(x, fill = T, use.names = T)
  x <- x[!(is.na(db_surname_firstname))]

  if (nrow(x) > 0){
    # Remove duplicate hits with same name
    x[, Name_score := stringdist::stringsim(Matched_surname_firstname, db_surname_firstname, method = "jaccard"), by = 1:nrow(x)]
    x[, Name_score_pos := stringdist::stringsim(Matched_surname_firstname, db_surname_firstname, method = "lv"), by = 1:nrow(x)]
    x <- x[with(x, ave(Name_score_pos, db_surname_firstname, FUN=max)==Name_score_pos),]
  } else{
    # Fill empty
    x[, Name_score := NA]
  }

  # Retrieve matches by DOB
  if (!(is.na(llaob$ll_DOB[i]))) {
    d <- as.character("")
    d <- agrep(llaob$ll_DOB[i],dbaob$db_DOB,
               ignore.case=TRUE, max.distance = 2, value=TRUE, useBytes = FALSE, costs = list(ins=2, del=2, sub=2))
    # Retain only best db matched DOB
    d <- data.table("db_DOB" = d)
    d[, DOB_score := stringdist::stringsim(as.character(llaob$ll_DOB[i]), db_DOB, method = "hamming")]
    d[, db_DOB := as.Date(lubridate::parse_date_time(db_DOB, orders = c("dmy", "mdy", "ymd")))]
  } else {
    # Fill missing list object
    d <- data.table(DOB_score=numeric())
  }
  return(list(x,d))
}



# Retrieve data function
#' Title
#'
#' @param i  Cycle number
#' @param case_column  column with unique identifier
#' @param llaob  line list database
#' @param dbaob  search datavase
#'
#' @return
#'
#' @examples
#'
#'  par_list <- retrieve_data(i, case_column, llaob, dbaob)
#'
#' @export
retrieve_data <- function(i, case_column, llaob, dbaob){
  setDT(llaob)
  # Name combinations for each person
  fn <- tstrsplit(llaob$ll_Name[i], " ")
  sn <- tstrsplit(llaob$ll_Surname[i], " ")
  sn_fun <- c(paste(llaob$ll_Surname[i], llaob$ll_Name[i], sep = " "), paste(rep(sn, each = length(fn)), fn, sep = " "), paste(rep(fn, each = length(sn)), sn, sep = " "))
  sn_fun <- sn_fun[!duplicated(sn_fun)]

  # Retrieve db name matches
  matches  <- fuz_search(dbaob, sn_fun)
  x <- matches[[1]]
  x <- unique(x[x$Name_score>=0.7,])
  d <- unique(matches[[2]])

  # If no matches next row
  if (nrow(x) == 0) {
    if (exists("unmatched")) {
      unmatched <- c(unmatched, i)
    } else {
      unmatched <- i
    }
    suppressWarnings(rm( list = Filter( exists, c("x", "d", " b", "fn", "sn", "sn_fun", "matches" ) ) ))#,"matches_reserve"

  } else {

    # Add ll to names found
    x[,ll_surname_firstname := llaob$ll_surname_firstname[i]]
    x[,ll_DOB := llaob$ll_DOB[i]]
    x <- merge(x, llaob[i,], all = T, by = c("ll_surname_firstname","ll_DOB"))
    setkey(x, "db_surname_firstname")

    # Retrieve db data based on name
    y <- copy(dbaob[dbaob$db_surname_firstname %in% c(x$db_surname_firstname),])
    setkey(y, "db_surname_firstname")
    y[, c(dba$sn, dba$fn, dba$dob, dba$pc) := NULL]
    y <- unique(y)
    y <- merge(y, x, by = "db_surname_firstname", all = T, allow.cartesian=TRUE)

    # Retrieve additional db data based on DOB
    if (nrow(d)>0) {
      b <- copy(dbaob[db_DOB %in% d$db_DOB])
      b[, c(dba$sn, dba$fn, dba$dob, dba$pc) := NULL]
      b[,ll_surname_firstname := llaob$ll_surname_firstname[i]]
      b[,ll_DOB := llaob$ll_DOB[i]]
      b[, Name_score := stringdist::stringsim(ll_surname_firstname, db_surname_firstname, method = "jaccard"), by = 1:nrow(b)]
      b[, Name_score_pos := stringdist::stringsim(ll_surname_firstname, db_surname_firstname, method = "lv"), by = 1:nrow(b)]
      b[, Matched_surname_firstname := "DOB match"]
      b <- b[Name_score >= 0.7, ]
      b <- merge(b, llaob[i,], all = T, by = c("ll_DOB","ll_surname_firstname"))
      y <- rbindlist(list(y,b), use.names = T, fill = T)
      y <- unique(y)
    }

    # Add match info
    y[,ll_Postcode := llaob$ll_Postcode[i]]
    y[,DOB_score := stringdist::stringsim(as.character(ll_DOB), as.character(db_DOB), method = "lv")]
    y[,PC_score := stringdist::stringsim(ll_Postcode, db_Postcode, method = "lv")]
    y[,Fuz_score := sum(Name_score, DOB_score, PC_score, na.rm = T), by = 1:nrow(y)]
    y <- y[with(y, order(Fuz_score, decreasing = T)), ]

    # Add case number
    if(missing(case_column)){
      case_column <- "Case"
      y[, (case_column):= i]
    } else {
      setDT(llaob)
      case_n <- as.character(llaob[i, ..case_column])
      suppressWarnings(y[, (case_column):= case_n])
    }

    # Remove duplicate hits for each case
    db_col <- names(y)[names(y) %in% names(dbaob)]
    dup <- copy(y[, .SD, .SDcols=db_col])
    y <- y[!(which(duplicated(dup) == TRUE)),]
    if("Name_score_pos" %in% names(y)) y[,Name_score_pos:= NULL]

    # Collect results
    if (exists("z")) {
      z <- rbindlist(list(z,y), fill = T, use.names = T)
    } else {
      z <- copy(y)
    }

    # Clear for next cycle
    suppressWarnings(rm( list = Filter( exists, c("x", "d", " b", "y", "fn", "sn", "sn_fun", "dup", "matches") ) ))
  }

  if (!exists("z")) {
    z <- data.table(ll_surname_firstname = NA)
  }
  if (!exists("unmatched")) {
    unmatched <- NA
  }
  setDT(z)

  par_return <- list(data = z, skipped = unmatched)
  return(par_return)
}


