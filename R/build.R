#' Build a dataset from derived
#'
#' This function builds a dataset out of the columns that just need to be pulled
#' through. So any variable that has a derivation in the format of
#' 'dataset.variable' will be pulled through to creat the new dataset. These
#' columns are often called 'Predecessors' in ADaM, but this is not univeral so
#' that is optional to specify.
#' @param metacore Metacore object that contains the specifications for the
#'   dataset of interest.
#' @param ds_list Named list of datsets that are needed to build the from
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#' @param predecessor_only By default `FALSE`, but if `TRUE` will only use
#'   derivations with the origin of 'Predecessor'
#'
#' @return datset
#' @export
#' @importFrom stringr str_to_lower str_detect str_extract str_to_upper
#' @importFrom dplyr filter pull mutate group_by group_split inner_join select full_join
#' @importFrom purrr map reduce
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' metacore <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#' select_dataset("ADSL")
#' ds_list <- list(DM =  read_xpt(metatools_example("dm.xpt")))
#' build_from_derived(metacore, ds_list)
build_from_derived <- function(metacore, ds_list, dataset_name = NULL,
                               predecessor_only = FALSE){
   metacore <- make_lone_dataset(metacore, dataset_name)
   derirvations <- metacore$derivations
   if(predecessor_only) {
      limited_dev_ids <- metacore$value_spec %>%
         filter(str_detect(.data$origin, "[P|p]redecessor")) %>%
         pull(.data$derivation_id)

      derirvations <- derirvations %>%
         filter(.data$derivation_id %in% limited_dev_ids)
   }
   vars_to_pull_through <- derirvations %>%
      filter(str_detect(.data$derivation, "^\\w*\\.[a-zA-Z0-9]*$"))
   # To lower so it is flexible about how people name their ds list
   vars_w_ds <- vars_to_pull_through %>%
      mutate(ds = str_extract(.data$derivation, "^\\w*(?=\\.)") %>%
                str_to_lower())
   ds_names <- vars_w_ds %>%
      pull(.data$ds) %>%
      unique()
   names(ds_list) <- names(ds_list) %>%
      str_to_lower()
   if(!all(ds_names %in% names(ds_list))){
      stop(paste0("Not all datasets provided. Please pass the following dataset(s):\n",
                  paste0(str_to_upper(ds_names), collapse = "\n"))
      )
   }
   join_by <- metacore$ds_vars %>%
      filter(!is.na(.data$key_seq)) %>%
      pull(.data$variable)
   vars_w_ds %>%
      mutate(col_name = str_extract(.data$derivation, "(?<=\\.).*")) %>%
      inner_join(metacore$value_spec, ., by = "derivation_id") %>%
      select(.data$variable, .data$ds, .data$col_name) %>%
      group_by(.data$ds) %>%
      group_split() %>%
      map(get_variables, ds_list) %>%
      reduce(full_join, by = join_by)

}



#' Internal functions to get variables from a dataset list
#'
#' This function is used with `build_from_derived` to build a dataset of columns
#' that are pulled directly from other datasets hris
#'
#' @param x Dataset with the old and new variable name and dataset name
#' @param ds_list List of datasets
#'
#' @return datasets
#' @noMd
get_variables <- function(x, ds_list){
   ds_name <- unique(x$ds)
   data <- ds_list[[ds_name]]
   rename_vec <- set_names(x$col_name, x$variable)
   data %>%
      select(x$col_name) %>%
      rename(rename_vec)
}


#' Drop Unspecified Variables
#'
#' This function drops all unspcifed variables. It will throw and error if the
#' dataset does not contain all expected variables.
#' @param data Dataset to change
#' @param metacore Metacore object that only contains the specifications for the
#'   dataset of interest.
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#' @importFrom dplyr pull across select
#' @importFrom purrr discard
#' @return Dataset with only specified columns
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(dplyr)
#' metacore <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
#'   select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt")) %>%
#'   mutate(foo = "Hello")
#' drop_unspec_vars(data, metacore)
drop_unspec_vars <- function(dataset, metacore, dataset_name = NULL){
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_list <- metacore$ds_vars %>%
      pull(.data$variable)
   to_drop <- names(dataset) %>%
      discard(~. %in% var_list)
   if(length(to_drop) > 1){
      out <- dataset %>%
         select(-all_of(to_drop))
   } else {
      out <- dataset
   }
   out
}
