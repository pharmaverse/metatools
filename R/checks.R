#' Check Control Terminology for a Single Column
#'
#' This function checks the column in the dataset only contains the control
#' terminology as defined by the metacore specification
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
#'   `FALSE`then NA will not be acceptable.
#'
#' @return Given data if column only contains control terms. If not, will error
#'   given the values which should not be in the column
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
check_ct_col <- function(data, metacore, var, na_acceptable = NULL) {
   verify_DatasetMeta(metacore)
   bad_vals <- get_bad_ct(data = data, metacore = metacore,
                          var = {{var}}, na_acceptable = na_acceptable)
   if(length(bad_vals) == 0){
      data
   } else {
      extra <- bad_vals %>%
         paste0("'", ., "'") %>%
         paste0(collapse = ", ")
      stop(paste("The following values should not be present:\n", extra))
   }
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
get_bad_ct <- function(data, metacore, var, na_acceptable = NULL){
   verify_DatasetMeta(metacore)
   col_name_str <- as_label(enexpr(var)) %>%
      str_remove_all("\"")
   if (!col_name_str %in% names(data)) {
      stop(paste(col_name_str, "not found in dataset. Please check and try again"), call. = FALSE)
   }
   ct <- get_control_term(metacore, {{ var }})
   if (is.vector(ct)) {
      check <- ct
   } else if ("code" %in% names(ct)) {
      check <- ct %>% pull(code)
   } else {
      stop("We currently don't have the ability to check against external libraries", call. = FALSE)
   }
   core <- metacore$ds_vars %>%
      filter(variable == col_name_str) %>%
      pull(core)
   attr(core, "label") <- NULL
   test <- ifelse(is.null(na_acceptable), !identical(core, "Required"), na_acceptable)
   if (test) {
      if (all(is.character(check))) {
         check <- c(check, NA_character_, "")
      } else {
         check <- c(check, NA)
      }
   }
   test <- pull(data, {{ var }}) %in% check
   pull(data, {{ var }})[!test] %>%
      unique()
}



#' Check Control Terminology for a Dataset
#'
#' This function checks that all columns in the dataset only contains the
#' control terminology as defined by the metacore specification
#' @param data Dataset to check
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest. If any variable has different codelists for different
#'   datasets the metacore object will need to be subsetted using
#'   `select_dataset` from the metacore package.
#' @param na_acceptable `logical` value or `character` vector, set to `NULL` by default.
#'   `NULL` sets the acceptability of missing values based on if the core for
#'   the variable is "Required" in the `metacore` object. If set to `TRUE` then will
#'   pass check if values are in the control terminology or are missing. If set
#'   to `FALSE` then NA will not be acceptable. If set to a `character` vector then
#'   only the specified variables may contain NA values.
#' @param omit_vars `character` vector indicating which variables should be skipped
#'   when doing the controlled terminology checks. Internally, `omit_vars` is
#'   evaluated before `na_acceptable`.
#'
#' @return Given data if all columns pass. It will error otherwise
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
#'}
check_ct_data <- function(data, metacore, na_acceptable = NULL, omit_vars = NULL) {
   verify_DatasetMeta(metacore)
   codes_in_data <- metacore$value_spec %>%
      filter(variable %in% names(data), !is.na(code_id)) %>%
      pull(code_id) %>%
      unique()
   # Remove any codes that have external libraries
   codes_to_check <- metacore$codelist %>%
      filter(type != "external_library", code_id %in% codes_in_data) %>%
      select(code_id)
   # convert list of codes to variables
   cols_to_check <- metacore$value_spec %>%
      inner_join(codes_to_check, by = "code_id", multiple = "all", relationship = "many-to-many") %>%
      filter(variable %in% names(data)) %>%
      pull(variable) %>%
      unique()

   # Subset cols_to_check by omit_vars
   if (is.character(omit_vars)) {
      check_vars_in_data(omit_vars, "omit_vars", data)
      cols_to_check <- setdiff(cols_to_check, omit_vars)
   }

   # send all variables through check_ct_col
   safe_chk <- safely(check_ct_col)

   if (is.character(na_acceptable)) {
      check_vars_in_data(na_acceptable, "na_acceptable", data)
      new_na_acceptable <- rep(FALSE, length(cols_to_check))
      new_na_acceptable[match(na_acceptable, cols_to_check)] <- TRUE

      results <- map2(cols_to_check, new_na_acceptable, function(x, naac) {
         out <- safe_chk(data, metacore, {{ x }}, naac)
         out$error
      })

   } else if(is.logical(na_acceptable) || is.null(na_acceptable)) {
      results <- cols_to_check %>%
         map(function(x) {
            out <- safe_chk(data, metacore, {{ x }}, na_acceptable)
            out$error
         })

   } else {
      stop("na_acceptable is not NULL, logical or character.", call. = FALSE)
   }


   # Write out warning message
   test <- map_lgl(results, is.null)
   if (all(test)) {
      return(data)
   } else {
      extras <- results %>%
         discard(is.null) %>%
         map(~.$message) %>%
         unlist() %>%
         str_remove("The following values should not be present:\n\\s")
      unique_test <- extras %>%
         keep(~str_detect(., "does not have a unique control term"))
      if(length(unique_test) > 0){
         stop(paste0(unique_test, collapse = "\n"), call. = FALSE)
      }
      message <- paste0(cols_to_check[!test], " (", extras, ")") %>%
         paste0(collapse = "\n")
      stop(paste0(
         "The following variables contained values not found in the control terminology
       Variable (Prohibited Value(s))\n",
         message),
         call. = FALSE)
   }
}

check_vars_in_data <- function(vars, vars_name, data) {
   if (!all(vars %in% names(data))) {
      stop(
         paste0(
            "Not all variables from ", vars_name, " are in the data: ",
            paste0(setdiff(vars, names(data)), collapse = ",")
         ),
         call. = FALSE)
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
#'   Note: Deprecated in version 1.0.0. The `dataset_name` argument will be removed
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
check_variables <- function(data, metacore, dataset_name = deprecated(), strict = TRUE) {
   if (is_present(dataset_name)) {
      lifecycle::deprecate_warn(
         when = "1.0.0",
         what = "check_variables(dataset_name)",
         details = cli_text("The {.arg dataset_name} argument will be removed in
                            a future release. Please use {.fcn metacore::select_dataset}
                            to subset the {.obj metacore} object to obtain metadata
                            for a single dataset.")
      )
      metacore <- make_lone_dataset(metacore, dataset_name)
   }
   verify_DatasetMeta(metacore)

   var_list <- metacore$ds_vars %>%
      filter(is.na(supp_flag) | !(supp_flag)) %>%
      pull(variable)

   missing <- var_list  %>% discard(~ . %in% names(data))
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
   output_string <- paste0("In: [", calling_function, "]" )

   for (i in seq_along(messages)) {
      message <- paste0(messages[i], ": ",
                       paste(data_list[[i]], collapse = ", "), sep = "\n")

      output_string <- paste(output_string, message, sep = "\n\n")
   }

   options(deparse.max.lines = 2000L)
   switch (as.character(strict),
      "TRUE"  = cli::cli_abort(output_string, call = NULL),
      "FALSE" = cli::cli_warn(output_string, call = NULL)
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
#'   Note: Deprecated in version 1.0.0. The `dataset_name` argument will be removed
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
         when = "1.0.0",
         what = "check_variables(dataset_name)",
         details = cli_text("The {.arg dataset_name} argument will be removed in
                            a future release. Please use {.fcn metacore::select_dataset}
                            to subset the {.obj metacore} object to obtain metadata
                            for a single dataset.")
      )
      metacore <- make_lone_dataset(metacore, dataset_name)
   }
   verify_DatasetMeta(metacore)
   keys <- get_keys(metacore,expr(!!metacore$ds_spec$dataset))
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
      stop(paste0("Keys do not uniquely identify records\n",
                  "variable keys:\n",
                  paste0(var_list, collapse = "\n")))
   }
   data
}
