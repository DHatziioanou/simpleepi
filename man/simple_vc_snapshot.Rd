% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_vc.R
\name{simple_vc_snapshot}
\alias{simple_vc_snapshot}
\title{Title Get a snapshot of previous database contents}
\usage{
simple_vc_snapshot(x = test$genotyping_variant_VC, date)
}
\arguments{
\item{x}{Flat version control column made from simple_version_control}

\item{date}{date of snapshot}
}
\value{
Returns snapshot of what database looked like for a given date
}
\description{
Title Get a snapshot of previous database contents
}
\examples{
# date <- "20220529"
# col <- "column_VC"
# df$Historic20220529 <- simple_vc_snapshot(x = df$data_VC, date = "20220529")
}
