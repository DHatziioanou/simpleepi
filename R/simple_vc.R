#' Title Compare two versions of data with the same length and mark new data in a version control column
#'
#' @param oldcol vector of previous data
#' @param newcol vector of new data
#' @param vccol existing or new vector to record changes in
#' @param olddate date of previous data for version control
#' @param newdate  date of new data for version control
#'
#' @return returns vccol newdate with new changes
#'
#' @examples
#'  dt =data.table(oldcol = c("A", "B", "C", NA, "D", "", "Test"),
#'                newcol = c("", "Test", NA, NA, "C", NA, "newdata"))
#'  dt$vc = simple_vc(oldcol = dt$oldcol, newcol = dt$newcol, vccol = dt$vccol)
#'
#'  dt$vccol = c("Original;A", "Original;B", " Original;C",  "Original;C then NA", " Original;D", NA, "no previous")
#'  dt$vcupdate = simple_vc(oldcol = dt$oldcol, newcol = dt$newcol, vccol = dt$vccol)
#'
#' @export
simple_vc <- function(oldcol = dt$oldcol, newcol = dt$newcol, vccol = dt$vccol, olddate = "Original", newdate = Sys.Date()){
  start <- Sys.time()
  if(length(oldcol) != length(newcol)) stop("Comparing unequal number of items")
  is.na(oldcol) <- oldcol == ""
  is.na(newcol) <- newcol == ""
  if(!exists("vccol")|is.null(vccol)) {
    vccol <- paste(olddate, oldcol, sep = ";")
    vccol[is.na(oldcol)] <- NA
  }
  newvc <- c()
  for(i in 1:length(newcol)){
    if(is.na(oldcol[i]) & is.na(newcol[i])) { # no data
      newvc <- c(newvc, vccol[i])
    } else if(is.na(oldcol[i] == newcol[i])) { # removed
      newvc <- c(newvc, paste(vccol[i], paste(newdate, newcol[i], sep = ";"), sep = ","))
    } else if(oldcol[i] == newcol[i]) { # same
      newvc <- c(newvc, vccol[i])
    }  else {
      newvc <- c(newvc, paste(vccol[i], paste(newdate, newcol[i], sep = ";"), sep = ",")) # changed
    }
  }
  end <- Sys.time(); runtime = end -start
  cat(paste("run time",round(runtime, 2), attr(runtime, "units")))
  return(newvc)
}

