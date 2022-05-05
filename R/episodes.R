#' Add episode information based on the duration of episodes
#'
#' @param unique_column Unique identifier column to summarise data by (string)
#'
#' @param time_column Date column to summarise data by
#'
#' @param duration Numeric duration of episodes in days
#' @param dt data
#'
#' @return Returns a data.table with summary
#' sample_time_diff = time difference between samples
#' New_episode = 1 for new episodes, 0 for repeat samples from the same episode.
#' episodeN = episode number for unique identifier
#' episode_time_diff = time difference from previous sample within episode
#' @importFrom rlang :=
#'
#' @examples
#'
#' @export
episodes <- function(dt, unique_column, time_column, duration){

  dt <- data.table::as.data.table(dt)
  dt <- dt[order(get(unique_column), get(time_column),decreasing=F)]
  dt[,sample_time_diff:= c(NA,as.numeric(base::diff(get(time_column), lag = 1))), by = c(unique_column)]
  dt[,New_episode := ifelse(sample_time_diff>duration|is.na(sample_time_diff), 1, 0), by = c(unique_column)]
  dt[,episodeN := cumsum(New_episode), by = c(unique_column)]
  dt[,episode_time_diff := c(NA,as.numeric(base::diff(get(time_column), lag = 1))), by = c(unique_column,"episodeN")]
  dt <- as.data.frame(dt)
  return(dt)
}
