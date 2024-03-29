% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packages.R
\name{package_replace}
\alias{package_replace}
\title{Package replacement with specified version of package}
\usage{
package_replace(
  package,
  version,
  repo = "https://cloud.r-project.org",
  force = FALSE,
  upgrade = "default",
  ...
)
}
\arguments{
\item{package}{name of package}

\item{version}{version to install}

\item{repo}{Optional repository. Default is CRAN.}

\item{force}{Optional force replacement if package version already installed. Default is FALSE}

\item{upgrade}{Optional should package dependencies be upgraded? One of "default", "ask", "always", or "never".}

\item{...}{Optional parameters from package remotes}
}
\value{
installs the desired package version
}
\description{
Package replacement with specified version of package
}
\examples{
# package_replace(package = "roxygen2", "7.1.2", force = TRUE)

}
