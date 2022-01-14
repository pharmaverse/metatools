#' Check Control Terminology for a Single Column
#'
#' This function checks the column in the dataset only contains the control
#' terminology as defined by the metacore specification
#'
#' @param data data to check
#' @param metacore a metacore object to get the codelist from. If the variable
#'   has different codelists for different datasets the metacore object will
#'   need to be subsetted using `select_dataset` from the metacore package.
#' @param col_name Name of column
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
#' spec <- define_to_MetaCore(metacore_example("ADaM_define.xml")) %>%
#'     select_dataset("ADSL")
#' data <- read_xpt(pkg_example("adsl.xpt"))
#' check_ct_col(data, spec, TRT01PN)
#' check_ct_col(data, spec, "TRT01PN")
check_ct_col <- function(data, metacore, col_name, na_acceptable = FALSE){
   col_name_str <- as_label(enexpr(col_name)) %>%
      str_remove_all("\"")
   if(!col_name_str %in% names(data)){
      stop(paste(col_name_str, "not found in dataset. Please check and try again"))
   }
   ct <- get_control_term(metacore, {{col_name}})
   if(is.vector(ct)) {
      check <- ct
   } else if("code" %in% names(ct)){
      check <- ct %>% pull(code)
   } else {
      stop("We currently don't have the ability to check against external libraries")
   }
   if(na_acceptable){
      if(all(is.character(check))){
         check <- c(check, NA_character_, "")
      } else {
         check <- c(check, NA)
      }
   }
   test <- pull(data, {{col_name}}) %in% check
   all(test)
}

#' Check Control Terminology for a Dataset
#'
#' This function checks that all columns in the dataset only contains the control
#' terminology as defined by the metacore specification
#' @param data dataset to check
#' @param metacore a subsetted metacore object. It should only have the in If the
#'   variable has different codelists for different datasets the metacore object
#'   will need to be subsetted using `select_dataset` from the metacore package.
#'
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter pull select inner_join
#' @return `TRUE` if all columns pass. It will error otherwise
#' @export
#'
#' @examples
#'  spec <- define_to_MetaCore(metacore_example("ADaM_define.xml")) %>%
#'     select_dataset("ADSL")
#' data <- haven::read_xpt(pkg_example("adsl.xpt"))
#' check_ct_data(data, spec, TRUE)
check_ct_data <- function(data, metacore,  na_acceptable = FALSE){
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
      inner_join(codes_to_check, by = "code_id") %>%
      filter(variable %in% names(data)) %>%
      pull(variable) %>%
      unique()
   # send all variables through check_ct_col
   results <- cols_to_check %>%
      map_lgl(function(x){
         check_ct_col(data, metacore, {{x}}, na_acceptable)
         })
   # Write out warning message
   if(all(results)){
      return(TRUE)
   } else {
      message <- cols_to_check[!results] %>%
         paste0(collapse = "\n")
      stop(paste0("The following variables contained values not found in the control terminology:\n", message))
   }
}
