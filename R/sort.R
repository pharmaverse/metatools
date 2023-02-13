
#' Sort Columns by Order
#'
#' This function sorts the dataset according to the order found in the
#' metacore object.
#' @param data Dataset to sort
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
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' order_cols(data, spec)
order_cols <- function(data, metacore, dataset_name = NULL) {
   metacore <- make_lone_dataset(metacore, dataset_name)

   var_ord <- metacore$ds_vars %>%
      filter(!is.na(order)) %>%
      arrange(order) %>%
      pull(variable) %>%
      keep(~. %in% names(data))

   data %>%
      select(all_of(var_ord), everything())
}


#' Sort Rows by Key Sequence
#'
#' This function sorts the dataset according to the key sequence found in the
#' metacore object.
#' @param data Dataset to sort
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
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' sort_by_key(data, spec)
sort_by_key <- function(data, metacore, dataset_name = NULL) {
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_ord <- metacore$ds_vars %>%
      filter(!is.na(key_seq)) %>%
      arrange(key_seq) %>%
      pull(variable)

   data %>%
      arrange(across(all_of(var_ord)))
}
