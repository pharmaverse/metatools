% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supp.R
\name{make_supp_qual}
\alias{make_supp_qual}
\title{Make Supplemental Qualifier}
\usage{
make_supp_qual(dataset, metacore, dataset_name = deprecated())
}
\arguments{
\item{dataset}{dataset the supp will be pulled from}

\item{metacore}{A subsetted metacore object to get the supp information from.
If not already subsetted then a \code{dataset_name} will need to be provided}

\item{dataset_name}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Optional string to
specify the dataset that is being built. This is only needed if the metacore
object provided hasn't already been subsetted.\cr
Note: Deprecated in version 0.2.0. The \code{dataset_name} argument will be removed
in a future release. Please use \code{metacore::select_dataset} to subset the
\code{metacore} object to obtain metadata for a single dataset.}
}
\value{
a CDISC formatted SUPP dataset
}
\description{
Make Supplemental Qualifier
}
\examples{

library(metacore)
library(safetyData)
library(tibble)
load(metacore_example("pilot_SDTM.rda"))
spec <- metacore \%>\% select_dataset("AE")
ae <- combine_supp(sdtm_ae, sdtm_suppae)
make_supp_qual(ae, spec) \%>\% as_tibble()
}
