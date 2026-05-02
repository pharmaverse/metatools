#' Check Control Terminology for a Single Column
#'
#' This function checks the column in the dataset only contains the control
#' terminology as defined by the metacore specification
#'
#' @param data Data to check
#'
#' @param metacore A metacore object to get the codelist from. If the variable
#'   has different codelists for different datasets the metacore object will
#'   need to be subsetted using `select_dataset` from the metacore package.
#'
#' @param var Name of variable to check
#'
#' @param na_acceptable Logical value, set to `NULL` by default, so the
#'   acceptability of missing values is based on if the core for the variable is
#'   "Required" in the `metacore` object. If set to `TRUE` then will pass check
#'   if values are in the control terminology or are missing. If set to
#'   `FALSE`then NA will not be acceptable.
#'
#' @param verbose `character` string controlling the verbosity of the output.
#'   Possible values are `"message"` (for general information and success messages)
#'   and `"warn"` (for warnings). Partial matching is allowed.
#'   **Important**: `"silent"` is explicitly **not** a valid option for `verbose` in
#'   this function. The primary purpose of `check_ct_data` is to identify and
#'   warn the user about non-compliant or problematic control terminology. Allowing
#'   the suppression of these warnings would bypass the function's intent and could
#'   lead to unnoticed data quality issues. If `verbose = "silent"` is provided, it will
#'   be coerced to `"message"` with a warning.
#'
#' @param .internal Logical value indicating whether the function is being
#'   called internally by another package function. If `TRUE`, the function
#'   suppresses user-facing messages and instead returns a logical indicator
#'   of whether any controlled terminology violations were detected. This
#'   argument is intended for internal use only and should not be set by
#'   end users.
#'
#' @return Given data if column only contains control terms. If not, will error
#'   given the values which should not be in the column
#'
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' check_ct_col(data, spec, TRT01PN)
#' check_ct_col(data, spec, "TRT01PN")
check_ct_col <- function(data, metacore, var, na_acceptable = NULL, verbose = "message", .internal = FALSE) {
  verify_DatasetMeta(metacore)

  # Verbose cannot be `silent` as the point of this function is to warn the user
  tryCatch(
    {
      arg <- match.arg(verbose, "silent")
      cli_warn(c(
        "x" = "Argument {.arg verbose} cannot be {.val {arg}} for {.fn check_ct_data}",
        "i" = "Must be one of: {.val message}, {.val warn}",
        "i" = "Defaulting to {.val message}"
      ))
      verbose <- "message"
    },
    error = function(e) {
       verbose <- validate_verbose(verbose, call = rlang::env_parent())
    }
  )

   var_name <- rlang::as_name(rlang::ensym(var))

   bad_vals <- get_bad_ct(
      data = data,
      metacore = metacore,
      var = {{ var }},
      na_acceptable = na_acceptable,
      .internal = TRUE
   )

   if (length(bad_vals) == 0) {
      if (.internal) {
         return(TRUE)
      } else if (verbose == "message") {
         return(data)
      }
      return(invisible(data))
   }

   if (is.list(bad_vals)) {
      msg <- unlist(lapply(names(bad_vals), function(nm) {
         vals <- paste0("'", bad_vals[[nm]], "'", collapse = ", ")
         paste0(nm, ": ", vals)
      }))

      cli_warn(c(
         "x" = "Invalid controlled terminology detected",
         "i" = "Variable: {var_name}",
         setNames(msg, rep("i", length(msg))),
         ""
      ))

   } else {
      codelist <- metacore$value_spec |>
         dplyr::filter(variable == var_name) |>
         dplyr::pull(code_id)

      cli_warn(c(
         "x" = "Invalid controlled terminology detected",
         "i" = "Variable: {var_name} | Codelist: {codelist}",
         "i" = "Values not permitted: {bad_vals}",
         ""
      ))
   }
   invisible(FALSE)
}

#' Gets vector of control terminology which should be there
#'
#' This function checks the column in the dataset only contains the control
#' terminology as defined by the metacore specification. It will return all
#' values not found in the control terminology
#'
#' @param data Data to check
#' @param metacore A metacore object to get the codelist from. If the variable
#'   has different codelists for different datasets the metacore object will
#'   need to be subsetted using `select_dataset` from the metacore package.
#' @param var Name of variable to check
#' @param na_acceptable Logical value, set to `NULL` by default, so the
#'   acceptability of missing values is based on if the core for the variable is
#'   "Required" in the `metacore` object. If set to `TRUE` then will pass check
#'   if values are in the control terminology or are missing. If set to
#'   `FALSE` then NA will not be acceptable.
#'
#' @return vector
#' @export
#'
#' @examples
#' library(haven)
#' library(metacore)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' get_bad_ct(data, spec, "DCSREAS")
#' get_bad_ct(data, spec, "DCSREAS", na_acceptable = FALSE)
#'
get_bad_ct <- function(data, metacore, var, na_acceptable = NULL, .internal = FALSE) {
   verify_DatasetMeta(metacore)
   col_name_str <- as_label(enexpr(var)) %>%
      str_remove_all("\"")

   if (!col_name_str %in% names(data)) {
      stop(paste(col_name_str, "not found in dataset. Please check and try again"), call. = FALSE)
   }

   ct <- get_control_term(metacore, {{ var }})

   core <- metacore$ds_vars %>%
      filter(variable == col_name_str) %>%
      pull(core)

   attr(core, "label") <- NULL

   na_ok <- ifelse(is.null(na_acceptable), !identical(core, "Required"), na_acceptable)

   # ---- CASE 1: No VLM ----
   if (is.data.frame(ct)) {

      check <- if (is.vector(ct)) {
         ct
      } else if ("code" %in% names(ct)) {
         ct %>% pull(code)
      } else {
         cli_warn(c(
            "x" = "Could not check controlled terminology for {.val {col_name_str}}",
            "i" = "We currently don't have the ability to check against external libraries. "
         ), call = rlang::env_parent())
         return(NULL)
      }

      if (na_ok) {
         check <- if (is.character(check)) c(check, NA_character_, "") else c(check, NA)
      }

      vals <- pull(data, {{ var }})
      bad_vals <- unique(vals[!vals %in% check]) |> format_blank_str()

      if (.internal) {
         return(bad_vals)
      }

      ct_name <- metacore$value_spec |> filter(variable == col_name_str) |> pull(code_id)

      cli_warn(c(
         "x" = "Invalid controlled terminology found",
         "i" = "Variable: {.val {col_name_str}} | Codelist: {.val {ct_name}}",
         "i" = "{bad_vals}"
      ))
   }

   # ---- CASE 2: VLM present ----
   if (is.list(ct)) {
      return(get_bad_ct_vlm(data, metacore, col_name_str, na_ok, .internal = TRUE))
   }
}

#' Get bad controlled terminology values for a variable with value level metadata
#'
#' Checks a variable against the controlled terminology defined by value level
#' metadata (VLM) in a metacore specification. For each VLM `where` clause
#' associated with the variable, the function subsets the data, retrieves the
#' relevant codelist, and returns any values found in the dataset that are not
#' permitted by that codelist.
#'
#' @param data Data to check.
#' @param metacore A metacore object containing the dataset and value level
#'   metadata specification.
#' @param var Name of the variable to check.
#' @param na_acceptable Logical scalar indicating whether missing values should be
#'   accepted. If `TRUE`, `NA` and `""` are treated as valid for character
#'   controlled terminology and `NA` for non-character controlled terminology.
#'
#' @return A named list containing only the VLM codelists with invalid values.
#'   Each element is named `"Codelist: <where_clause>"` and contains the unique
#'   invalid values found for that VLM condition. If no invalid values are found,
#'   an empty list is returned.
#'
#' @details
#' The function currently supports only VLM conditions using the `EQ` operator.
#' Any other operator triggers a warning and is skipped.
#'
#' @export
get_bad_ct_vlm <- function(data, metacore, var, na_acceptable = NULL, .internal = FALSE) {
   bad_vals <- c()
   where_clauses <- get_vlm_where(metacore, var)

   for (where_clause in where_clauses) {

      # Parse e.g. "PARAMCD EQ ADURD"
      parts <- stringr::str_split(where_clause, " ", simplify = TRUE)
      var_name <- parts[1]
      op       <- parts[2]
      value    <- parts[3]

      # Only EQ supported
      if (op != "EQ") {
         warning("Only EQ conditions currently supported")
         next
      }

      # Subset data based on VLM condition
      subset_data <- dplyr::filter(data, .data[[var_name]] == value)

      if (nrow(subset_data) == 0) next

      # Get CT for this specific VLM slice
      ct_sub <- get_control_term(metacore, {{ var }}, where = where_clause)

      check <- dplyr::pull(ct_sub, code)

      if (na_acceptable) {
         check <- if (is.character(check)) {
            c(check, NA_character_, "")
         } else {
            c(check, NA)
         }
      }

      vals <- dplyr::pull(subset_data, .data[[var]])
      bad_vals[[paste("Codelist:", where_clause)]] <- unique(vals[!vals %in% check])
   }

   bad_vals <- bad_vals[lengths(bad_vals) > 0]
   if (.internal) {
      return(invisible(bad_vals))
   }

   msg <- unlist(lapply(names(bad_vals), function(nm) {
      vals <- paste0("'", bad_vals[[nm]], "'", collapse = ", ")
      paste0(nm, ": ", vals)
   }))

   cli_warn(c(
      "x" = "Invalid controlled terminology detected",
      "i" = "Variable: {var_name}",
      setNames(msg, rep("i", length(msg))),
      ""
   ))
   return()
}


#' Check Control Terminology for a Dataset
#'
#' This function checks that all columns in the dataset only contains the
#' control terminology as defined by the metacore specification.
#'
#' @param data Dataset to check
#'
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest. If any variable has different codelists for different
#'   datasets the metacore object will need to be subsetted using
#'   `select_dataset` from the metacore package.
#'
#' @param na_acceptable `logical` value or `character` vector, set to `NULL` by default.
#'   `NULL` sets the acceptability of missing values based on if the core for
#'   the variable is "Required" in the `metacore` object. If set to `TRUE` then will
#'   pass check if values are in the control terminology or are missing. If set
#'   to `FALSE` then NA will not be acceptable. If set to a `character` vector then
#'   only the specified variables may contain NA values.
#'
#' @param omit_vars `character` vector indicating which variables should be skipped
#'   when doing the controlled terminology checks. Internally, `omit_vars` is
#'   evaluated before `na_acceptable`.
#'
#' @param verbose `character` string controlling the verbosity of the output.
#'   Possible values are `"message"` (for general information and success messages)
#'   and `"warn"` (for warnings). Partial matching is allowed.
#'   **Important**: `"silent"` is explicitly **not** a valid option for `verbose` in
#'   this function. The primary purpose of `check_ct_data` is to identify and
#'   warn the user about non-compliant or problematic control terminology. Allowing
#'   the suppression of these warnings would bypass the function's intent and could
#'   lead to unnoticed data quality issues. If `verbose = "silent"` is provided, it will
#'   be coerced to `"message"` with a warning.
#'
#' @return Given data if all columns pass. It will issue a warning otherwise.
#'
#' @export
#'
#' @examples
#' library(haven)
#' library(metacore)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
#' data <- read_xpt(metatools_example("adsl.xpt"))
#'
#' check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"))
#' \dontrun{
#' # These examples produce errors:
#' check_ct_data(data, spec, na_acceptable = FALSE)
#' check_ct_data(data, spec, na_acceptable = FALSE, omit_vars = "DISCONFL")
#' check_ct_data(data, spec, na_acceptable = c("DSRAEFL", "DCSREAS"), omit_vars = "DISCONFL")
#' }
check_ct_data <- function(data, metacore, na_acceptable = NULL, omit_vars = NULL, verbose = "message") {
  verify_DatasetMeta(metacore)

  # Verbose cannot be `silent` as the point of this function is to warn the user
  tryCatch(
    {
      arg <- match.arg(verbose, "silent")
      cli_warn(c(
        "x" = "Argument {.arg verbose} cannot be {.val {arg}} for {.fn check_ct_data}",
        "i" = "Must be one of: {.val message}, {.val warn}",
        "i" = "Defaulting to {.val message}"
      ))
      verbose <- "message"
    },
    error = function(e) {
      verbose <- validate_verbose(verbose, call = rlang::env_parent())
    }
  )

  codes_in_data <- metacore$value_spec %>%
    dplyr::filter(variable %in% names(data), !is.na(code_id)) %>%
    dplyr::pull(code_id) %>%
    unique()

  # Remove any codes that have external libraries
  codes_to_check <- metacore$codelist %>%
    dplyr::filter(type != "external_library", code_id %in% codes_in_data) %>%
    dplyr::select(code_id)

  # Convert list of codes to variables
  cols_to_check <- metacore$value_spec %>%
    dplyr::inner_join(codes_to_check, by = "code_id", relationship = "many-to-many") %>%
    dplyr::filter(variable %in% names(data)) %>%
    dplyr::pull(variable) %>%
    unique()

  # Subset cols_to_check by omit_vars
  if (is.character(omit_vars)) {
    check_vars_in_data(omit_vars, "omit_vars", data)
    cols_to_check <- setdiff(cols_to_check, omit_vars)
  }

  # Validate na_acceptable
  if (!is.null(na_acceptable) &&
    !is.logical(na_acceptable) &&
    !is.character(na_acceptable)) {
    cli::cli_abort(
      "na_acceptable must be NULL, logical, or character."
    )
  }

  # Run checks and collect flags
  results <- purrr::map_lgl(cols_to_check, function(x) {
    if (is.character(na_acceptable)) {
      na_flag <- x %in% na_acceptable
    } else if (is.logical(na_acceptable) || is.null(na_acceptable)) {
      na_flag <- na_acceptable
    }

    check_ct_col(data, metacore, !!rlang::sym(x), na_flag, "message", .internal = TRUE)
  })

  # If no warnings triggered
  if (all(results) && verbose == "message") {
    cli::cli_inform(c(
      "v" = "All controlled terminology checks passed"
    ))
  }

  # Print dataset Y/N?
  if (verbose == "message") {
    return(data)
  }

  invisible(data)
}

check_vars_in_data <- function(vars, vars_name, data) {
  if (!all(vars %in% names(data))) {
    stop(
      paste0(
        "Not all variables from ", vars_name, " are in the data: ",
        paste0(setdiff(vars, names(data)), collapse = ",")
      ),
      call. = FALSE
    )
  }
  return(NULL)
}

#' Check Variable Names
#'
#' This function checks the variables in the dataset against the variables
#' defined in the metacore specifications. If everything matches the function
#' will print a message stating everything is as expected. If there
#' are additional or missing variables an error will explain the discrepancies
#' @param data Dataset to check
#' @param metacore metacore object that only contains the specifications for the
#'   dataset of interest.
#' @param dataset_name `r lifecycle::badge("deprecated")` Optional string to
#'   specify the dataset. This is only needed if the metacore object provided
#'   hasn't already been subsetted.\cr
#'   Note: Deprecated in version 0.2.0. The `dataset_name` argument will be removed
#'   in a future release. Please use `metacore::select_dataset` to subset the
#'   `metacore` object to obtain metadata for a single dataset.
#' @param strict A logical value indicating whether to perform strict
#'   validation on the input dataset. If \code{TRUE} (default), errors will be raised
#'   if validation fails. If \code{FALSE}, warnings will be issued instead, allowing
#'   the function execution to continue event with invalid data.
#'
#' @return message if the dataset matches the specification and the dataset, and error otherwise
#' @export
#'
#' @examples
#' library(haven)
#' library(metacore)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' check_variables(data, spec)
#' data["DUMMY_COL"] <- NA
#' check_variables(data, spec, strict = FALSE)
check_variables <- function(data, metacore, dataset_name = deprecated(), strict = FALSE) {
  if (is_present(dataset_name)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "check_variables(dataset_name)",
      details = cli_inform(c("i" = col_red("The {.arg dataset_name} argument will be removed in a future release.
      Please use {.fn metacore::select_dataset} to subset the {.obj metacore} object to obtain
      metadata for a single dataset.")))
    )
    metacore <- make_lone_dataset(metacore, dataset_name)
  }
  verify_DatasetMeta(metacore)

  var_list <- metacore$ds_vars %>%
    filter(is.na(supp_flag) | !(supp_flag)) %>%
    pull(variable)

  missing <- var_list %>% discard(~ . %in% names(data))
  extra <- names(data) %>% discard(~ . %in% var_list)

  messages <- character(0)
  data_list <- list()

  if (length(missing) > 0) {
    messages <- c(messages, "The following variables are missing")
    data_list <- c(data_list, list(missing))
  }

  if (length(extra) > 0) {
    messages <- c(messages, "The following variables do not belong")
    data_list <- c(data_list, list(extra))
  }

  if (length(messages) > 0) {
    print_to_console(messages, data_list, strict = {{ strict }})
  } else {
    message("No missing or extra variables")
  }

  data
}

#' Print Messages to Console
#'
#' This function prints formatted messages to the console, either as errors (stopping
#' execution) or as warnings. It is designed as a helper function to provide informative
#' messages during validation checks.
#'
#' @param messages A character vector of messages to be printed. Each element corresponds
#'   to a separate message.
#' @param data_list A list of character vectors. Each element in the list corresponds
#'   to a message in `messages` and provides associated data (e.g., column names).
#'   If an element in `messages` has no corresponding data, include a `NULL`.
#' @param strict A logical value indicating whether to print messages as
#'   errors (\code{TRUE}, default) or warnings (\code{FALSE}).
#'
#' @details The function constructs a formatted message string including the calling
#' function's name, the individual messages provided in `messages`, and associated data
#' from `data_list`. The function uses \code{switch} to call either `stop()` or `warning()`
#' based on `strict` and prints the full message string to the console.
#'
#' @return None. The function's primary purpose is its side effect of printing a message.
#' It does not return a meaningful value.
#'
#' @noRd
#'
print_to_console <- function(messages, data_list, strict = TRUE) {
  calling_function <- paste(deparse(sys.call(-1)), collapse = " ")
  output_string <- paste0("In: [", calling_function, "]")

  for (i in seq_along(messages)) {
    message <- paste0(messages[i], ": ",
      paste(data_list[[i]], collapse = ", "),
      sep = "\n"
    )

    output_string <- paste(output_string, message, sep = "\n\n")
  }

  options(deparse.max.lines = 2000L)
  switch(as.character(strict),
    "TRUE"  = cli::cli_abort(c(output_string), call = NULL),
    "FALSE" = cli::cli_warn(c(output_string), call = NULL)
  )
}

#' Check Uniqueness of Records by Key
#'
#' This function checks the uniqueness of records in the dataset by key using
#' `get_keys` from the metacore package. If the key uniquely identifies each
#' record the function will print a message stating everything is as expected.
#' If records are not uniquely identified an error will explain the duplicates.
#' @param data Dataset to check
#' @param metacore metacore object that only contains the specifications for the
#'   dataset of interest.
#' @param dataset_name `r lifecycle::badge("deprecated")` Optional string to
#'   specify the dataset that is being built. This is only needed if the metacore
#'   object provided hasn't already been subsetted.\cr
#'   Note: Deprecated in version 0.2.0. The `dataset_name` argument will be removed
#'   in a future release. Please use `metacore::select_dataset` to subset the
#'   `metacore` object to obtain metadata for a single dataset.
#'
#' @return message if the key uniquely identifies each dataset record, and error otherwise
#' @export
#'
#' @examples
#' library(haven)
#' library(metacore)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' check_unique_keys(data, spec)
check_unique_keys <- function(data, metacore, dataset_name = deprecated()) {
  if (is_present(dataset_name)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "check_unique_keys(dataset_name)",
      details = cli_inform(c("i" = col_red("The {.arg dataset_name} argument will be removed in a future release.
      Please use {.fn metacore::select_dataset} to subset the {.obj metacore} object to obtain
      metadata for a single dataset.")))
    )
    metacore <- make_lone_dataset(metacore, dataset_name)
  }
  verify_DatasetMeta(metacore)
  keys <- get_keys(metacore, expr(!!metacore$ds_spec$dataset))
  var_list <- keys %>%
    pull(variable)
  missing <- var_list %>%
    discard(~ . %in% names(data))
  if (length(missing) > 0) {
    stop(paste0(
      "The following variable keys are missing in the dataset:\n",
      paste0(missing, collapse = "\n")
    ))
  }
  grouped <- data %>%
    group_by(pick(!!keys$variable)) %>%
    add_count() %>%
    filter(.data$n != 1)
  if (nrow(grouped) == 0) {
    message("Keys uniquely identify records")
  } else {
    stop(paste0(
      "Keys do not uniquely identify records\n",
      "variable keys:\n",
      paste0(var_list, collapse = "\n")
    ))
  }
  data
}
