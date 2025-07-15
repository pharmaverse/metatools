
#' Sort Columns by Order
#'
#' This function sorts the dataset according to the order found in the
#' metacore object.
#' @param data Dataset to sort
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest.
#' @param dataset_name `r lifecycle::badge("deprecated")` Optional string to
#'   specify the dataset that is being built. This is only needed if the metacore
#'   object provided hasn't already been subsetted.\cr
#'   Note: Deprecated in version 0.2.0. The `dataset_name` argument will be removed
#'   in a future release. Please use `metacore::select_dataset` to subset the
#'   `metacore` object to obtain metadata for a single dataset.
#'
#' @return dataset with ordered columns
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' order_cols(data, spec)
order_cols <- function(data, metacore, dataset_name = deprecated()) {
   if (is_present(dataset_name)) {
      lifecycle::deprecate_warn(
         when = "0.2.0",
         what = "order_cols(dataset_name)",
         details = cli_inform(c("i" = col_red("The {.arg dataset_name} argument will be removed in a future release.
      Please use {.fn metacore::select_dataset} to subset the {.obj metacore} object to obtain
      metadata for a single dataset.")))
      )
      metacore <- make_lone_dataset(metacore, dataset_name)
   }
   verify_DatasetMeta(metacore)
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
#' @param dataset_name `r lifecycle::badge("deprecated")` Optional string to
#'   specify the dataset that is being built. This is only needed if the metacore
#'   object provided hasn't already been subsetted.\cr
#'   Note: Deprecated in version 0.2.0. The `dataset_name` argument will be removed
#'   in a future release. Please use `metacore::select_dataset` to subset the
#'   `metacore` object to obtain metadata for a single dataset.
#'
#' @return dataset with ordered columns
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt"))
#' sort_by_key(data, spec)
sort_by_key <- function(data, metacore, dataset_name = deprecated()) {
   if (is_present(dataset_name)) {
      lifecycle::deprecate_warn(
         when = "0.2.0",
         what = "sort_by_key(dataset_name)",
         details = cli_inform(c("i" = col_red("The {.arg dataset_name} argument will be removed in a future release.
      Please use {.fn metacore::select_dataset} to subset the {.obj metacore} object to obtain
      metadata for a single dataset.")))
      )
      metacore <- make_lone_dataset(metacore, dataset_name)
   }
   verify_DatasetMeta(metacore)
   var_ord <- metacore$ds_vars %>%
      filter(!is.na(key_seq)) %>%
      arrange(key_seq) %>%
      pull(variable)

   data %>%
      arrange(across(all_of(var_ord)))
}
