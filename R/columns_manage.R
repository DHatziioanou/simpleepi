#' Title Column names list duplicates in 1 or more objects. Disregards case.
#'
#' @param x list of objects
#'
#' @return Returns list with same = duplicates in same object, other = duplicates between objects
#'
#' @examples
#' # colnamecheck(df)
#' # colnamecheck(list(df1, df2, df3))
#'
#' @export
col_namecompare <- function(x) {
  n <- lapply(x, names)
  nl <- lapply(n, tolower)

  # duplicates in same and other file
  ds <- lapply(nl, duplicated)
  do <- duplicated(unlist(nl))

  dup <- list(same = NA, other = NA)
  # duplicate names same ob
  if(sum(unlist(ds))>0) {
    p1 <- Map(`[`, n, ds)
    dup[["same"]] <- p1
  }
  # duplicate names other ob
  if(sum(do)>0) {
    unlist(nl)[do]
    p2 <- lapply(n, function(x) x[grepl(paste0(unlist(nl)[do], collapse = "|"), ignore.case = T, x)])
    dup[["other"]] <- p2
  }
  return(dup)
}


#' Title Column name search for string patterns
#'
#' @param dt  object such as dataframe, datatable, list
#' @param strings one or more strings to search for
#' @param out return column names or positions
#'
#' @return returns column names or positions
#'
#' @examples
#' # col_nameregex(dt,"dob", out = "names")
#'
#' @export
col_nameregex <- function(dt, strings, out = "names"){
  if(length(strings)>1) strings <-  paste0(strings, collapse = "|")
  x <- names(dt)[grepl(strings, ignore.case = T, names(dt))]
  if(out == "names") {
    return(x)
  } else {
    pos <- unlist(lapply(x,function(x) which( colnames(dt)==x)))
    return(pos)
  }
}


#' Title Combine vectors or table columns while keeping all values.
#'
#' @param l left hand side column
#' @param r right hand side column
#' @param case Default of NULL does not formatting. Options upper, lower or title format data before combining.
#'
#' @return returns vector of data combined by position
#'
#' @examples
#' # columns_combine(df$unique_id, df$forename)
#'
#' @export
combine_vectors <- function(l, r, case = NULL){

  # Manage class differences by turning incompatible ones into character class
  if(class(l) != class(r)) {
    l <- as.character(l)
    r <- as.character(r)
  }
  # format case, encoding
  if(!is.null(case)){
    l <- simpleencode(l, case)
    r <- simpleencode(r, case)
  }
  out <- l

  # Combine where both cols have data
  comb <- which(!(is.na(l)) & (!is.na(r)) )
  identical <- which(l == r)
  if(length(identical)!=0){
    comb <- comb[! comb %in% identical]
  }
  combined <- paste(l[comb],r[comb], sep = ", ")
  if(length(comb)!=0) out[comb] <- combined

  # Add 2nd where 1st col is empty
  add <- which((is.na(l)) & (!is.na(r)))
  if(length(add)!=0) out[add] <- r[add]

  return(out)
}


#' Title Compare vector contents to a expected categories
#'
#' @param data vector to perform check on
#' @param cat categorical values to look for
#'
#' @return returns list containing summary of comparison, whether data fully conforms to categories and whether data has missing of additional categories
#'
#' @examples
#'
#' # check1 <- QC_categories(df$coolumn1, cat = c("low", "medium", "high"))
#'
#' @export
QC_categories <- function(data, cat){
  data <- as.vector(unique(data))
  if(is.null(data)) stop("input is empty")
  cat <- as.vector(cat)
  if(is.null(cat)) stop("no categories")
  # All data match categories?
  c <- all(sort(data) %in% sort(cat))
  # Any additional in data?
  a <- data[!(data %in% cat)]
  # Any missing from data?
  m <- cat[!(cat %in% data)]
  out <- list(summary = NA,data_conforms =c,additional_categories=a,missing_categories=m)
  # no categories found?
  if(sum(data %in% cat) == 0){
    QC <- "FALSE - Categories not found"
  } else {
    # All match and present
    if(isTRUE(c) & length(m)==0){
      QC <- "TRUE Data matches categories perfectly"
    } else if(isTRUE(c) & length(m)>0){
      QC <- paste0("TRUE Data matches categories, ", length(m), " categories missing; ",paste0(m, collapse = ", "))
    } else if(!isTRUE(c)){

      if(length(m)==0){
        QC <- paste0("FALSE All categories present, ", length(a), " additional categories in data; ",paste0(a, collapse = ", "))
      } else {
        QC <- paste0("FALSE Categories missing; ", paste0(m, collapse = ", "), " additional categories found; ",paste0(a, collapse = ", "))
      }
    }
  }
  out$summary <- QC
  return(out)
}

#' Title Create table of data.table column characteristics
#'
#' @param dt data.table. If a dataframe is provided it will be converted to a datatable.
#'
#' @return returns a data.table with column length metrics min, max, mean, median, na metrics, class, skewness and kurtosis per column.
#'
#' @examples
#' # formats_dt <- simple_format_calatogue(dt)
#'
#'
#' @export
simple_format_calatogue <- function(dt){

  if(!"data.table" %in% class(dt)) dt <- data.table::as.data.table(dt)
  cat(paste("Of", ncol(dt), "columns processing:"))
  dt[dt==""] <- NA
  for(c in 1:ncol(dt)){

    cat(c) ; cat(".")
    c_name = names(dt)[c]
    cat(c_name); cat("..")
    nullcol =  all(is.na(dt[,get(c_name)]))
    na_any = ifelse(nullcol, TRUE, any(is.na(dt[,get(c_name)])))
    clengthswidth = nchar(dt[,get(c_name)], allowNA = T, keepNA = T, type = "width")

    if(c == 1){
      format_table <- data.table::data.table(col_name = c_name,
                                 Rclass = class(dt[,get(c_name)]),
                                 na_all = nullcol,
                                 na_any = na_any,
                                 na_percent = ifelse(nullcol, 100, fifelse(na_any, sum(is.na(dt[,get(c_name)]))/length(clengthswidth),0)),
                                 length_min = ifelse(nullcol, as.numeric(NA), round(min(clengthswidth, na.rm = T), 2)),
                                 length_max = ifelse(nullcol, as.numeric(NA), round(max(clengthswidth, na.rm = T), 2)),
                                 length_mean = ifelse(nullcol, as.numeric(NA), round(mean(clengthswidth, na.rm = T), 2)),
                                 length_median = ifelse(nullcol, as.numeric(NA), round(median(clengthswidth, na.rm = T), 2)),
                                 skewness_SAS_SPSS = ifelse(nullcol, as.numeric(NA),
                                                            ifelse(length(clengthswidth[!is.na(clengthswidth)])>3, e1071::skewness(clengthswidth, na.rm = T, type = 2), as.numeric(NA))),
                                 kurtosis_SAS_SPSS = ifelse(nullcol, as.numeric(NA),
                                                            ifelse(length(clengthswidth[!is.na(clengthswidth)])>3, e1071::kurtosis(clengthswidth,na.rm = T, type = 2), as.numeric(NA))))

    } else {
      format_table = data.table::rbindlist(list(format_table,
                                    data.table::data.table(col_name = c_name,
                                               Rclass = class(dt[,get(c_name)]),
                                               na_all = nullcol,
                                               na_any = na_any,
                                               na_percent = ifelse(nullcol, 100, fifelse(na_any, sum(is.na(dt[,get(c_name)]))/length(clengthswidth),0)),
                                               length_min = ifelse(nullcol, as.numeric(NA), round(min(clengthswidth, na.rm = T), 2)),
                                               length_max = ifelse(nullcol, as.numeric(NA), round(max(clengthswidth, na.rm = T), 2)),
                                               length_mean = ifelse(nullcol, as.numeric(NA), round(mean(clengthswidth, na.rm = T), 2)),
                                               length_median = ifelse(nullcol, as.numeric(NA), round(median(clengthswidth, na.rm = T), 2)),
                                               skewness_SAS_SPSS = ifelse(nullcol, as.numeric(NA),
                                                                          ifelse(length(clengthswidth[!is.na(clengthswidth)])>3, e1071::skewness(clengthswidth, na.rm = T, type = 2), as.numeric(NA))),
                                               kurtosis_SAS_SPSS = ifelse(nullcol, as.numeric(NA),
                                                                          ifelse(length(clengthswidth[!is.na(clengthswidth)])>3, e1071::kurtosis(clengthswidth,na.rm = T, type = 2), as.numeric(NA))))
      ), use.names = T, fill = T)

    }

  }
  cat("done")
  return(format_table)
}
