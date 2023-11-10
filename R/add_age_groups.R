#' Title Add age group column to data.frame
#'
#' @param age  age vector
#' @param groups Input vector of age ranges in format "N1-N2" where N1 is the lower age and N2 the upper age. Mark range where max is infinite as "N1+"
#' @param unknowns vector of Age values to be considered unknowns eg -1, NA, "not stated"
#' @param factor Logical; should returned age groups be formatted as factor? Default is FALSE.
#'
#' @return Returns age groups
#'
#' @examples
#' # df <- data.frame(ID = c(1:4), Age = c(8,-1,38,26))
#' # groups <- c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+")
#' # df$group <- age_groups(df$Age, groups, unknowns, factor)
#'
#' @import data.table
#' @export
age_groups <- function(age, groups= c('0-4', "5-10", "11-16", "17-24", "25-30", "31-39", "40-59", "60-79", "80+"), unknowns = c("-1", NA), factor = FALSE){
  if(missing(age)) stop("No age data")
  .a <- data.table::tstrsplit(groups, "-", fill=NA, type.convert=F, names=c("L","R"), fixed=TRUE)
  .a <- data.table::data.table(L = .a$L, R = .a$R)
  .a[,L := as.numeric(gsub("+", "", L, fixed = T))][,R:=as.numeric(gsub("+", "", R, fixed = T))][grep("+",groups, fixed = T),R := Inf]
  .a <- cbind(.a, groups)
  out <- sapply(age, FUN = function(x) .a[L <= x & R>=x,]$groups, simplify = TRUE)
  out[sapply(out,function(x) length(x)==0 | any(x %in% unknowns))]<- "unknown age"
  out <- unlist(out)
  if(isTRUE(factor)) out <- factor(out, c(groups, "unknown age"))
  return(out)
}


#' Title Calculate ONS populations by age group for any age grouping using ONS populations file from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea21principalprojectionukpopulationinagegroups
#'
#' @param populations object containing ONS populations
#' @param age_groups  data.frame containing age group categories to calculate populations for
#'
#' @return
#'  exports a data.frame with aggregated population numbers by age group
#'
#' @examples
#'
#'  # Create age grouping
#'  # groups <- data.frame(under18 = c(0,17),
#'            # workingage = c(18,65),
#'            # over65 = c(66,"90+"),
#'            # stringsAsFactors = FALSE)
#'  # ONS_populations_3categories <- ONS_age_groups(ONS, groups)
#'
#' @export
ONS_age_groups <- function(populations, age_groups){

  populations <- as.data.frame(populations)

  # Add age group populations
  for(i in names(age_groups)){

    # Populations inclusing 90+
    if(age_groups[2,i] == "90+" & age_groups[1,i] != "90+"){

      populations[[i]] <- rowSums(populations[,c(which(names(populations) %in% c(as.character(as.numeric(age_groups[1,i]):89), "90+")))], na.rm = T)

      # Populations under 90
    } else if (age_groups[2,i] == "90+" & age_groups[1,i] == "90+"){

      populations[[i]] <- populations[,c(which(names(populations) ==  "90+"))]

      # Populations under 90
    } else {

      populations[[i]] <- rowSums(populations[,c(which(names(populations) %in% c(as.character(as.numeric(age_groups[1,i]):as.numeric(age_groups[2,i])))))], na.rm = T)

    }

  }

  # Shave unnecessary columns off
  try(
    df <- populations[, c(colnames(populations)[1:grep("All", colnames(populations), ignore.case = T)], names(age_groups))]
  )

  if(inherits(df, "try-error")){
    df <- df[, c(colnames(df)[1:grep("Population", colnames(df), ignore.case = T)], names(age_groups))]
  }

  names(df) <- c(colnames(df)[1:(grep("All", colnames(df), ignore.case = T)-1)], "Population all ages", names(age_groups))

  # Return age group df
  return(df)
}







