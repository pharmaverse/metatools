% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R
\name{add_variables}
\alias{add_variables}
\title{Add Missing Variables}
\usage{
add_variables(dataset, metacore, dataset_name = deprecated())
}
\arguments{
\item{dataset}{Dataset to add columns to. If all variables are present no
columns will be added.}

\item{metacore}{metacore object that only contains the specifications for the
dataset of interest.}

\item{dataset_name}{Optional string to specify the dataset. This is only
needed if the metacore object provided hasn't already been subsetted.\cr
Note: Deprecated in version 0.2.0. The \code{dataset_name} argument will be removed
in a future release. Please use \code{metacore::select_dataset} to subset the
\code{metacore} object to obtain metadata for a single dataset.}
}
\value{
The given dataset with any additional columns added
}
\description{
This function adds in missing columns according to the type set in the
metacore object. All values in the new columns will be missing, but typed
correctly. If unable to recognize the type in the metacore object will return
a logical type.
}
\examples{
library(metacore)
library(haven)
library(dplyr)
load(metacore_example("pilot_ADaM.rda"))
spec <- metacore \%>\% select_dataset("ADSL")
data <- read_xpt(metatools_example("adsl.xpt")) \%>\%
   select(-TRTSDT, -TRT01P, -TRT01PN)
add_variables(data, spec)
}
