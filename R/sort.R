
#' Sort Columns by Order
#'
#' This function sorts the dataset according to the order found in the
#' metacore object.
#' @param data dataset to sort
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest.
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#'
#' @return dataset with ordered columns
#' @export
#' @importFrom metacore select_dataset
#' @importFrom dplyr filter arrange pull select all_of everything
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#'     select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' sort_order(data, spec)
sort_order <- function(data, metacore, dataset_name = NULL){
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_ord <- metacore$ds_vars %>%
      filter(!is.na(.data$order)) %>%
      arrange(.data$order) %>%
      pull(.data$variable)
   data %>%
      select(all_of(var_ord), everything())
}


#' Sort Rows by Key Sequence
#'
#' This function sorts the dataset according to the key sequence found in the
#' metacore object.
#' @param data dataset to sort
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest.
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#'
#' @return dataset with ordered columns
#' @export
#' @importFrom metacore select_dataset
#' @importFrom dplyr filter arrange pull select all_of everything across
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#'     select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' sort_key(data, spec)
sort_key <- function(data, metacore, dataset_name = NULL){
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_ord <- metacore$ds_vars %>%
      filter(!is.na(.data$key_seq)) %>%
      arrange(.data$key_seq) %>%
      pull(.data$variable)

   data %>%
      arrange(across(var_ord))
}
