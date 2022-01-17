#' Build a dataset from derived
#'
#' This function builds a dataset out of the columns that just need to be pulled
#' through. So any variable that has a derivation in the format of
#' 'dataset.variable' will be pulled through to creat the new dataset. These
#' columns are often called 'Predecessors' in ADaM, but this is not univeral so
#' that is optional to specify.
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest.
#' @param ds_list Named list of datsets that are needed to build the from
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#' @param predecessor_only by default `FALSE`, but if `TRUE` will only use
#'   derivations with the origin of 'Predecessor'
#'
#' @return datset
#' @export
#'
#' @examples
#' library(metacore)
#' metacore <- define_to_MetaCore(metacore_example("ADaM_define.xml")) %>%
#' select_dataset("ADSL")
#' ds_list <- list(DM =  read_xpt(pkg_example("dm.xpt")))
#' build_from_derived(metacore, ds_list)
build_from_derived <- function(metacore, ds_list, dataset_name = NULL,
                               predecessor_only = FALSE){
   metacore <- make_lone_dataset(metacore, dataset_name)
   derirvations <- metacore$derivations
   if(predecessor_only) {
      limited_dev_ids <- metacore$value_spec %>%
         filter(str_detect(origin, "[P|p]redecessor")) %>%
         pull(derivation_id)

      derirvations <- derirvations %>%
         filter(derivation_id %in% limited_dev_ids)
   }
   vars_to_pull_through <- derirvations %>%
      filter(str_detect(derivation, "^\\w*\\.[a-zA-Z0-9]*$"))
   # To lower so it is flexible about how people name their ds list
   vars_w_ds <- vars_to_pull_through %>%
      mutate(ds = str_extract(derivation, "^\\w*(?=\\.)") %>%
                str_to_lower())
   ds_names <- vars_w_ds %>%
      pull(ds) %>%
      unique()
   names(ds_list) <- names(ds_list) %>%
      str_to_lower()
   if(!all(ds_names %in% names(ds_list))){
      stop(paste0("Not all datasets provided. Please pass the following dataset(s):\n",
                  paste0(str_to_upper(ds_names), collapse = "\n"))
      )
   }
   join_by <- metacore$ds_vars %>%
      filter(!is.na(key_seq)) %>%
      pull(variable)
   vars_w_ds %>%
      mutate(col_name = str_extract(derivation, "(?<=\\.).*")) %>%
      inner_join(metacore$value_spec, ., by = "derivation_id") %>%
      select(variable, ds, col_name) %>%
      group_by(ds) %>%
      group_split() %>%
      map(get_variables, ds_list) %>%
      reduce(full_join, by = join_by)

}





#' Internal functions to get variables from a dataset list
#'
#' This function is used with `build_from_derived` to build a dataset of columns
#' that are pulled directly from other datasets hris
#'
#' @param x dataset with the old and new variable name and dataset name
#' @param ds_list list of datasets
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
