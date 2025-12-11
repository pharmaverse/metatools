#' Get path to pkg example
#'
#' pkg comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' metatools_example()
#' metatools_example("dm.xpt")
metatools_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "metatools"))
  } else {
    system.file("extdata", file, package = "metatools", mustWork = TRUE)
  }
}


#' Convert metacore object to just a single dataset
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated as of version 0.2.0 and will be removed in a future
#' version. Dataset subsetting must now be performed via the `select_dataset`
#' function of the `metacore` package.
#'
#' @param metacore metacore object, which may or may not be subsetted
#' @param dataset_name Name of datasets which may or may not be null. If it is
#'   not null then it will be used to subset.
#'
#' @return metacore object
#' @noRd
make_lone_dataset <- function(metacore, dataset_name) {
  lifecycle::deprecate_soft(
    what = "make_lone_dataset()",
    when = "0.2.0"
  )
  if (!(nrow(metacore$ds_spec) == 1 | !is.null(dataset_name))) {
    stop("Requires either a subsetted metacore object or a dataset name", call. = FALSE)
  }
  if (!is.null(dataset_name)) {
    metacore <- select_dataset(metacore, dataset_name)
  }
  metacore
}

#' Check if messages should be displayed
#' @param verbose Verbosity level
#' @noRd
should_message <- function(verbose) {
  verbose == "message"
}

#' Check if warnings should be displayed
#' @param verbose Verbosity level
#' @noRd
should_warn <- function(verbose) {
  verbose %in% c("message", "warn")
}

#' Validate verbose parameter
#' @param verbose Verbosity level to validate
#' @noRd
validate_verbose <- function(verbose, arg = rlang::caller_arg(verbose), call = rlang::caller_env()) {
   choices <- c("message", "warn", "silent")
   tryCatch(
      match.arg(verbose, choices),
      error = function(e) {
         cli_abort(c(
            "x" = "{.arg {arg}} should be one of: {cli::ansi_collapse(choices, last = ', ')}"
         ), call = call)
      }
   )
}
