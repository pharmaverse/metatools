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
#' @param na_acceptable Logical value, set to `FALSE` by default, meaning
#'   missing values are not acceptable. If set to `TRUE` then will pass check if
#'   values are in the control terminology or are missing
#'
#' @importFrom metacore get_control_term
#' @importFrom dplyr pull
#' @importFrom stringr str_remove_all
#' @return logical value, `TRUE` if it passes and `FALSE` if it fails
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#'   select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' check_ct_col(data, spec, TRT01PN)
#' check_ct_col(data, spec, "TRT01PN")
check_ct_col <- function(data, metacore, var, na_acceptable = FALSE) {
  col_name_str <- as_label(enexpr(var)) %>%
    str_remove_all("\"")
  if (!col_name_str %in% names(data)) {
    stop(paste(col_name_str, "not found in dataset. Please check and try again"))
  }
  ct <- get_control_term(metacore, {{ var }})
  if (is.vector(ct)) {
    check <- ct
  } else if ("code" %in% names(ct)) {
    check <- ct %>% pull(.data$code)
  } else {
    stop("We currently don't have the ability to check against external libraries")
  }
  if (na_acceptable) {
    if (all(is.character(check))) {
      check <- c(check, NA_character_, "")
    } else {
      check <- c(check, NA)
    }
  }
  test <- pull(data, {{ var }}) %in% check
  all(test)
}

#' Check Control Terminology for a Dataset
#'
#' This function checks that all columns in the dataset only contains the
#' control terminology as defined by the metacore specification
#' @param data Dataset to check
#' @param metacore Metacore object that contains the specifications for the
#'   dataset of interest. If any variable has different codelists for different
#'   datasets the metacore object will need to be subsetted using
#'   `select_dataset` from the metacore package.
#' @param na_acceptable Logical value, set to `FALSE` by default, meaning
#'   missing values are not acceptable. If set to `TRUE` then will pass check if
#'   values are in the control terminology or are missing
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter pull select inner_join
#' @return `TRUE` if all columns pass. It will error otherwise
#' @export
#'
#' @examples
#' library(haven)
#' library(metacore)
#' library(magrittr)
#' spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#'   select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' check_ct_data(data, spec, TRUE)
check_ct_data <- function(data, metacore, na_acceptable = FALSE) {
  codes_in_data <- metacore$value_spec %>%
    filter(.data$variable %in% names(data), !is.na(.data$code_id)) %>%
    pull(.data$code_id) %>%
    unique()
  # Remove any codes that have external libraries
  codes_to_check <- metacore$codelist %>%
    filter(.data$type != "external_library", .data$code_id %in% codes_in_data) %>%
    select(.data$code_id)
  # convert list of codes to variables
  cols_to_check <- metacore$value_spec %>%
    inner_join(codes_to_check, by = "code_id") %>%
    filter(.data$variable %in% names(data)) %>%
    pull(.data$variable) %>%
    unique()
  # send all variables through check_ct_col
  results <- cols_to_check %>%
    map_lgl(function(x) {
      check_ct_col(data, metacore, {{ x }}, na_acceptable)
    })
  # Write out warning message
  if (all(results)) {
    return(TRUE)
  } else {
    message <- cols_to_check[!results] %>%
      paste0(collapse = "\n")
    stop(paste0("The following variables contained values not found in the control terminology:\n", message))
  }
}


#' Check Variable Names
#'
#' This function checks the variables in the dataset against the variables
#' defined in the metacore specifications. If everything matches the function
#' will return `TRUE` and a message starting everything is as expected. If there
#' are additional or missing variables and error will explain the discrepancies
#' @param data Dataset to check
#' @param metacore Metacore object that only contains the specifications for the
#'   dataset of interest.
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#'
#' @return message if the dataset matches the specification, and error otherwise
#' @export
#' @importFrom metacore select_dataset
#' @importFrom purrr discard
#' @importFrom dplyr pull
#'
#' @examples
#' library(haven)
#' library(metacore)
#' library(magrittr)
#' spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#'   select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' check_variables(data, spec)
check_variables <- function(data, metacore, dataset_name = NULL) {
  metacore <- make_lone_dataset(metacore, dataset_name)
  var_list <- metacore$var_spec %>%
    pull(.data$variable)
  missing <- var_list %>%
    discard(~ . %in% names(data))
  extra <- names(data) %>%
    discard(~ . %in% var_list)
  if (length(missing) == 0 & length(extra) == 0) {
    message("No missing or extra variables")
    TRUE
  } else if (length(missing) > 0 & length(extra) > 0) {
    stop(paste0(
      "The following variables are missing:\n",
      paste0(missing, collapse = "\n"),
      "\nThe following variables do not belong:\n",
      paste0(extra, collapse = "\n")
    ))
  } else if (length(missing) > 0) {
    stop(paste0(
      "The following variables are missing:\n",
      paste0(missing, collapse = "\n")
    ))
  } else {
    stop(paste0(
      "The following variables do not belong:\n",
      paste0(extra, collapse = "\n")
    ))
  }
}
