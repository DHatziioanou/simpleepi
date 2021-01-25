#' Title Calculate ONS populations by age group for any age grouping using the ONS populations file
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
#'  # groups <- data.frame(under18 = c(0,17), workingage = c(18,65), over65 = c(66,"90+"), stringsAsFactors = FALSE)
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






