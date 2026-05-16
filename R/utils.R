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
   browser()
  choices <- c("message", "warn", "silent")

  if (!is.null(disallow)) {
     choices <- setdiff(choices, disallow)

     if (verbose %in% disallow) {
        cli::cli_warn(c(
           "x" = "Argument {.arg {arg}} cannot be {.val {verbose}}.",
           "i" = "Must be one of: {.val {choices}}.",
           "i" = "Defaulting to {.val {choices[[1]]}}."
        ))
        return(choices[[1]])
     }
  }

  tryCatch(
    match.arg(verbose, choices),
    error = function(e) {
      cli_abort(c(
        "x" = "{.arg {arg}} should be one of: {cli::ansi_collapse(choices, last = ', ')}"
      ), call = call)
    }
  )
}

get_vlm_where <- function(metacore, var, dataset = NULL) {
   vs <- metacore$value_spec

   if (!is.null(dataset)) {
      vs <- dplyr::filter(vs, .data$dataset == dataset)
   }

   vs %>%
      dplyr::filter(.data$variable == var) %>%
      dplyr::pull(.data$where) %>%
      unique() %>%
      stats::na.omit()
}

format_blank_str <- function(x) {
   x_chr <- as.character(x)
   x_chr[x_chr == ""] <- '""'
   x_chr
}

strip_dot_prefix <- function(x) {
   sub(".*\\.", "", x)
}

build_vlm_filter <- function(where_clause) {

   parts <- stringr::str_split(where_clause, "\\s+", simplify = TRUE)

   var <- parts[1]
   op  <- toupper(parts[2])
   val <- paste(parts[3:length(parts)], collapse = " ")
   val <- trimws(val)

   op_map <- c(
      EQ = "==", "=" = "==", "==" = "==",
      NE = "!=", "!=" = "!=",
      GT = ">",  ">"  = ">",
      LT = "<",  "<"  = "<",
      GE = ">=", ">=" = ">=",
      LE = "<=", "<=" = "<=",
      IN = "%in%",
      NOTIN = "!%in%"
   )

   op_resolved <- unname(op_map[op])

   if (is.na(op_resolved)) {
      cli_warn(c(
         "x" = "The operator {.val {op}} found in the VLM where clause {.val {where}} is not a valid CDISC operator",
         "i" = "Please check the {.var where} column of your {.var metacore$value_spec} table",
         "i" = "Checks against the controlled terminology for the column {.var {var}} will be skipped",
         "i" = "You can use the {.arg omit_vars} argument to disable checks for this variable"
      ))
      return(NULL)
   }

   # ---- IN / NOTIN ----
   if (op %in% c("IN", "NOTIN")) {

      vals <- parse_vlm_values(val)

      expr <- rlang::expr(.data[[!!var]] %in% !!vals)

      if (op == "NOTIN") {
         expr <- rlang::expr(! (!!expr))
      }

      return(expr)
   }

   # ---- scalar comparisons ----
   is_num <- suppressWarnings(!is.na(as.numeric(val)))
   val_parsed <- if (is_num) as.numeric(val) else val

   rlang::call2(
      op_resolved,
      rlang::expr(.data[[!!var]]),
      val_parsed
   )
}

parse_vlm_values <- function(val) {

   val <- trimws(val)

   # VLM passed with correct syntax, already c(...)
   if (grepl("^c\\s*\\(.*\\)$", val)) {

      expr <- rlang::parse_expr(val)
      vals <- as.list(expr)[-1]

      out <- vapply(vals, function(x) {
         if (is.symbol(x)) {
            as.character(x)
         } else {
            rlang::as_string(x)
         }
      }, character(1))

      return(out)
   }

   # Try to correct invalid syntax
   vals <- stringr::str_remove_all(val, "[()]")
   vals <- stringr::str_split(vals, "[,\\s]+")[[1]]
   vals <- vals[vals != ""]

   vals
}
