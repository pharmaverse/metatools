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
check_message <- function(verbose) {
  verbose == "message"
}

#' Check if warnings should be displayed
#' @param verbose Verbosity level
#' @noRd
check_warn <- function(verbose) {
  verbose %in% c("message", "warn")
}

#' Validate a verbosity argument
#'
#' Validates a verbosity option against the supported values
#' `"message"`, `"warn"`, and `"silent"`. Specific values can be
#' excluded using the `invalid` argument.
#'
#' This is primarily intended for internal argument validation.
#'
#' @param verbose A character scalar specifying the verbosity level.
#'   Must be one of `"message"`, `"warn"`, or `"silent"`,
#'   unless excluded via `invalid`.
#' @param invalid Optional character vector of verbosity values to
#'   disallow. Any supplied values are removed from the set of valid
#'   choices before validation.
#' @param arg The name of the argument being validated. Used for
#'   error messaging.
#' @param call The execution environment used for error reporting.
#'
#' @return
#' A character scalar containing the validated verbosity value.
#'
#' @examples
#' validate_verbose("message")
#'
#' validate_verbose("warn")
#'
#' # Disallow "silent"
#' validate_verbose("silent", invalid = "silent")
#'
#' # Restrict to only "message"
#' validate_verbose("message", invalid = c("warn", "silent"))
#'
#' @noRd
validate_verbose <- function(verbose, disallow = NULL, arg = rlang::caller_arg(verbose),
                             call = rlang::caller_env()) {
   choices <- c("message", "warn", "silent")

   tryCatch({
      match <- match.arg(verbose, choices)

      if (match %in% disallow) {
         choices <- setdiff(choices, disallow)
         cli::cli_warn(c(
            "x" = "Argument {.arg {arg}} cannot be {.val {verbose}}.",
            "i" = "Must be one of: {.val {choices}}.",
            "i" = "Defaulting to {.val {choices[[1]]}}."
         ))
         return(choices[[1]])
      }
   },
   error = function(e) {
      cli_abort(c(
         "x" = "{.arg {arg}} should be one of: {.val {choices}}"
      ), call = call)
   }
   )
   match
}

format_blank_str <- function(x) {
   x_chr <- as.character(x)
   x_chr[x_chr == ""] <- '""'
   x_chr
}

strip_dot_prefix <- function(x) {
   sub(".*\\.", "", x)
}
