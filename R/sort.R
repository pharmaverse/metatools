
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
#' spec <- define_to_MetaCore(metacore_example("ADaM_define.xml")) %>%
#'     select_dataset("ADSL")
#' data <- haven::read_xpt(pkg_example("adsl.xpt"))
#' order_sort(data, spec)
order_sort <- function(data, metacore, dataset_name = NULL){
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_ord <- metacore$ds_vars %>%
      filter(!is.na(order)) %>%
      arrange(order) %>%
      pull(variable)
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
#' @importFrom dplyr filter arrange pull select all_of everything
#'
#' @examples
#' library(metacore)
#' spec <- define_to_MetaCore(metacore_example("ADaM_define.xml")) %>%
#'     select_dataset("ADSL")
#' data <- haven::read_xpt(pkg_example("adsl.xpt"))
#' key_sort(data, spec)
key_sort <- function(data, metacore, dataset_name = NULL){
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_ord <- metacore$ds_vars %>%
      filter(!is.na(key_seq)) %>%
      arrange(key_seq) %>%
      pull(variable)

   data %>%
      arrange(across(var_ord))
}
