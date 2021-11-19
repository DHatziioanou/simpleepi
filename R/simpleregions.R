#' Tidy English PHEC names for consistency of formatting
#'
#' @param x vector with English PHEC region names
#'
#' @return Returns x formatted to English PHEC names from ONS.
#'
#' @examples
#'  data = data.frame(regions =c("UNKNOWN", "YORKSHIRE AND THE HuMBER", "East Of England"),
#'  id = c(1,2,3))
#'  region_names = data.frame(regions = unique(data$regions))
#'  region_names$regions_clean =  simpleregions(region_names$regions)
#'  data <- merge(data, region_names, by = "regions", all = T)
#'
#' @export
simpleregions <- function(x) {
  d <- data.table::data.table(tofix = unique(x))
  library(data.table)
  d[,new := ""]
  d[grepl("IRELAND|NI|ISLE OF MAN|ISLANDS|SC|WALES|ISLE", tofix, ignore.case = T), new := "Other DA"]
  d[grepl("Not found|Unknown", tofix, ignore.case = T), new := "Unknown"]
  d[grepl("YORKSHIRE", tofix, ignore.case = T), new := "Yorkshire and Humber"]
  d[grepl("North", tofix, ignore.case = T) & grepl("West", tofix, ignore.case = T), new :="North West"]
  d[grepl("North", tofix, ignore.case = T) & grepl("East", tofix, ignore.case = T), new :="North East"]
  d[grepl("South", tofix, ignore.case = T) & grepl("East", tofix, ignore.case = T), new :="South East"]
  d[grepl("South", tofix, ignore.case = T) & grepl("West", tofix, ignore.case = T), new :="South West" ]
  d[grepl("Midlands", tofix, ignore.case = T) & grepl("West", tofix, ignore.case = T), new :="West Midlands"]
  d[grepl("Midlands", tofix, ignore.case = T) & grepl("East", tofix, ignore.case = T), new :="East Midlands" ]
  d[grepl("England", tofix, ignore.case = T) & grepl("East", tofix, ignore.case = T), new :="East of England"]
  d[grepl("London", tofix, ignore.case = T), new :="London" ]
  unmatched <- d[new == "", "tofix" ]
  if(nrow(unmatched) >0) warning(paste0("Unmatched regions found; ", paste0(unmatched$tofix, collapse = ", ")), call. = F)
  return(d$new)
}
