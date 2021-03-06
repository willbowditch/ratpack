% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratpack.R
\name{import_user_packages}
\alias{import_user_packages}
\title{Add users installed packages to the external.packages option of packrat
This means that package sources wont be included in the packrat, so best
to run packrat::set_opts(external.packages=NULL) before packrat::snapshot}
\usage{
import_user_packages()
}
\description{
Add users installed packages to the external.packages option of packrat
This means that package sources wont be included in the packrat, so best
to run packrat::set_opts(external.packages=NULL) before packrat::snapshot
}
\examples{
import_user_packages()
}
