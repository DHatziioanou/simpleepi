#' Title Calculate frequency of line list row counts by geography and date variables
#'
#' @param data                line list with a geography and date column
#' @param geography_column    Variable with geography
#' @param time_column         Variable with dates
#'
#' @return         returns data with additional EMA, SMA and change variables
#'
#' @examples
#'
#'
#' @export
frequency_of_rows_by_geography_time <- function(data, geography_column, time_column){
  if (!require("data.table")) {
    install.packages("data.table")
    library(data.table)
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require("lubridate")) {
    install.packages("lubridate")
    library(lubridate)
  }
  if (!require("pscl")) {
    install.packages("pscl")
    library(pscl)
  }

  setDF(data)

  # time_column format
  if (!is.Date(data[,time_column]) & !is.numeric(data[,time_column])) {
    if (!all(is.na(data[, time_column] <- lubridate::parse_date_time(data[, time_column], orders = c("dmy", "mdy", "ymd"))))) {
      data[, time_column] <- lubridate::parse_date_time(data[, time_column], orders = c("dmy", "mdy", "ymd"))
    } else if (!all(is.na(data[, time_column] <- as.numeric(data[, time_column] )))) {
      data[, time_column] <- as.numeric(data[, time_column] )
    } else {stop("Time column needs to be a date or a number") }
  }

  # Cases missing geography or time
  message(paste(
    NROW(data[is.na(geography_column) | is.na(time_column) | geography_column == "" | time_column == "" , ]),
    "rows missing geography and/or time and will not be counted"
  ))

  dt_count <- data %>%
    group_by_at(vars(geography_column, time_column)) %>%
    count() %>%
    ungroup()
  #  colnames(dt_count)[3] <-  print(geography_column)
  #  dt_count <- dt_count[complete.cases(dt_count),]

  setDT(dt_count)

  # Add missing time points
  setkeyv(dt_count, c(
    names(dt_count)[names(dt_count) == geography_column],
    names(dt_count)[names(dt_count) == time_column]))
  if (is.Date(time_column)){
    dt_count <- dt_count[CJ(unique(dt_count[,get(geography_column)]),
                            seq(
                              min(dt_count[,get(time_column)]),
                              max(dt_count[,get(time_column)]),
                              by = "day"))]
  } else {
    dt_count <- dt_count[CJ(unique(dt_count[,get(geography_column)]),
                            seq(
                              min(dt_count[,get(time_column)]),
                              max(dt_count[,get(time_column)]),
                              by = 1))]
  }

  setDF(dt_count)
  dt_count[is.na(dt_count$n),"n"] <- 0
  dt_count <- dt_count[with(dt_count, order(get(geography_column), get(time_column), decreasing = F)), ]

  # Add EMA and SMA averages for period
  dt_count <- dt_count %>%
    group_by_at(geography_column) %>%
    mutate(changen = (n - lag(n,1))/(lag(n,1))) %>%   # % CHANGE FROM PREVIOUS TIMEPOINT
    mutate(moveavEMA = EMA(n, n = 7)) %>%  # Exponentially-weighted mean, giving more weight to recent observations
    group_by_at(geography_column) %>%
    mutate(moveavEMAdiff = (moveavEMA - lag(moveavEMA,7))) %>% # Daily difference of EMA from previous week
    mutate(moveavSMA = SMA(n, n = 7)) %>%  # 7 DAYS  arithmetic mean
    mutate(moveavSMAdiff = (moveavSMA - lag(moveavSMA,7)))  # Daily difference of arithmetic mean from previous week

  # Make freq table with only last period
  dt_count <- as.data.frame(dt_count)
  dt_count$moveavEMAdiff_end <- dt_count$moveavEMAdiff
  dt_count$moveavEMAdiff_end[
    dt_count[,time_column] <
      (max(as.data.frame(dt_count)[,time_column], na.rm = TRUE) - 7) ] <- NA
  dt_count$moveavEMAdiff_enddir <- ifelse(dt_count$moveavEMAdiff_end > 0, "Positive", "Negative")


  return(dt_count_full)
}
