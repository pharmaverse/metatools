% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_variables}
\alias{check_variables}
\title{Check Variable Names}
\usage{
check_variables(data, metacore, dataset_name = deprecated(), strict = TRUE)
}
\arguments{
\item{data}{Dataset to check}

\item{metacore}{metacore object that only contains the specifications for the
dataset of interest.}

\item{dataset_name}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Optional string to
specify the dataset. This is only needed if the metacore object provided
hasn't already been subsetted.\cr
Note: Deprecated in version 0.2.0. The \code{dataset_name} argument will be removed
in a future release. Please use \code{metacore::select_dataset} to subset the
\code{metacore} object to obtain metadata for a single dataset.}

\item{strict}{A logical value indicating whether to perform strict
validation on the input dataset. If \code{TRUE} (default), errors will be raised
if validation fails. If \code{FALSE}, warnings will be issued instead, allowing
the function execution to continue event with invalid data.}
}
\value{
message if the dataset matches the specification and the dataset, and error otherwise
}
\description{
This function checks the variables in the dataset against the variables
defined in the metacore specifications. If everything matches the function
will print a message stating everything is as expected. If there
are additional or missing variables an error will explain the discrepancies
}
\examples{
library(haven)
library(metacore)
library(magrittr)
load(metacore_example("pilot_ADaM.rda"))
spec <- metacore \%>\% select_dataset("ADSL")
data <- read_xpt(metatools_example("adsl.xpt"))
check_variables(data, spec)
data["DUMMY_COL"] <- NA
check_variables(data, spec, strict = FALSE)
}
