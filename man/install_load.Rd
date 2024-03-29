% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packages.R
\name{install_load}
\alias{install_load}
\title{For a list of packages install missing ones and load them all.}
\usage{
install_load(packages, ..., repo, lib, quiet)
}
\arguments{
\item{packages}{vector of required package names}

\item{...}{Other packages}

\item{repo}{repository path (optional)}

\item{lib}{library path (optional)}

\item{quiet}{True or False}
}
\value{
Named packages installed and loaded
}
\description{
For a list of packages install missing ones and load them all.
}
\examples{

# install_load("data.table")
# install_load("data.table", "dplyr", "tidyr", "ggplot2")

}
