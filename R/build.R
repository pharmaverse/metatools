#' Build a dataset from derived
#'
#' This function builds a dataset out of the columns that just need to be pulled
#' through. So any variable that has a derivation in the format of
#' 'dataset.variable' will be pulled through to create the new dataset. When
#' there are multiple datasets present, they will be joined by the shared
#' `key_seq` variables. These columns are often called 'Predecessors' in ADaM,
#' but this is not universal so that is optional to specify.
#' @param metacore metacore object that contains the specifications for the
#'   dataset of interest.
#' @param ds_list Named list of datasets that are needed to build the from
#' @param dataset_name Optional string to specify the dataset that is being
#'   built. This is only needed if the metacore object provided hasn't already
#'   been subsetted.
#' @param predecessor_only By default `FALSE`, but if `TRUE` will only use
#'   derivations with the origin of 'Predecessor'
#' @param keep Boolean to determine if the original columns should be kept. By
#'   default `FALSE`, so only the ADaM columns are kept. If `TRUE` the resulting
#'   dataset will have all the ADaM columns as well as any SDTM column that were
#'   renamed in the ADaM (i.e `ARM` and `TRT01P` will be in the resulting
#'   dataset)
#'
#' @return dataset
#' @export
#' @importFrom stringr str_to_lower str_detect str_extract str_to_upper
#' @importFrom dplyr filter pull mutate group_by group_split inner_join select
#'   full_join bind_rows
#' @importFrom tidyr unnest
#' @importFrom purrr map reduce
#' @importFrom tibble tibble
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))
#' build_from_derived(spec, ds_list, predecessor_only = FALSE)
build_from_derived <- function(metacore, ds_list, dataset_name = NULL,
                               predecessor_only = TRUE, keep = FALSE) {
   metacore <- make_lone_dataset(metacore, dataset_name)
   derirvations <- metacore$derivations %>%
      mutate(derivation = trimws(derivation))

   if (predecessor_only) {
      limited_dev_ids <- metacore$value_spec %>%
         filter(str_detect(str_to_lower(origin), "predecessor")) %>%
         pull(derivation_id)

      derirvations <- derirvations %>%
         filter(derivation_id %in% limited_dev_ids)
      if (nrow(derirvations) == 0) {
         stop("No presecessor variables found please check your metacore object")
      }
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
   if (!all(ds_names %in% names(ds_list))) {
      stop(paste0(
         "Not all datasets provided. Please pass the following dataset(s):\n",
         paste0(str_to_upper(ds_names), collapse = "\n")
      ))
   }

   ds_keys <- metacore$ds_vars %>%
      filter(!is.na(key_seq)) %>%
      pull(variable)

   joining_vals_to_add <- ds_list %>%
      map(function(x){
         names(x) %>%
            keep(~ . %in% ds_keys)
         })

   join_by = joining_vals_to_add %>%
      reduce(intersect)
   additional_vals <- tibble(ds = names(ds_list),
          variable = joining_vals_to_add) %>%
      unnest(variable) %>%
      mutate(col_name = variable)

   vars_w_ds %>%
      mutate(col_name = str_extract(derivation, "(?<=\\.).*")) %>%
      inner_join(metacore$value_spec, ., by = "derivation_id") %>%
      select(variable, ds, col_name) %>%
      bind_rows(additional_vals) %>%
      group_by(ds) %>%
      group_split() %>%
      map(get_variables, ds_list, keep) %>%
      reduce(full_join, by = join_by)
}



#' Internal functions to get variables from a dataset list
#'
#' This function is used with `build_from_derived` to build a dataset of columns
#' that are pulled directly from other datasets
#'
#' @param x Dataset with the old and new variable name and dataset name
#' @param ds_list List of datasets
#' @param keep boolean if old columns should be kept
#'
#' @return datasets
#' @noRd
get_variables <- function(x, ds_list, keep) {
   ds_name <- unique(x$ds)
   data <- ds_list[[ds_name]]
   rename_vec <- set_names(x$col_name, x$variable)
   if (keep) {
      out <- data %>%
         select(x$col_name) %>%
         mutate(across(rename_vec))
   } else {
      out <- data %>%
         select(x$col_name) %>%
         rename(all_of(rename_vec))
   }
   out
}


#' Drop Unspecified Variables
#'
#' This function drops all unspecified variables. It will throw and error if the
#' dataset does not contain all expected variables.
#' @param dataset Dataset to change
#' @param metacore metacore object that only contains the specifications for the
#'   dataset of interest.
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#' @importFrom dplyr pull across select filter
#' @importFrom purrr discard
#' @return Dataset with only specified columns
#' @export
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(dplyr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt")) %>%
#'   select(USUBJID, SITEID) %>%
#'   mutate(foo = "Hello")
#' drop_unspec_vars(data, spec)
drop_unspec_vars <- function(dataset, metacore, dataset_name = NULL) {
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_list <- metacore$ds_vars %>%
      filter(is.na(supp_flag) | !(supp_flag)) %>%
      pull(variable)
   to_drop <- names(dataset) %>%
      discard(~ . %in% var_list)
   if (length(to_drop) > 0) {
      out <- dataset %>%
         select(-all_of(to_drop))
      message(paste0("The following variable(s) were dropped:\n  ",
              paste0(to_drop, collapse = "\n  ")))
   } else {
      out <- dataset
   }
   out
}



#' Add Missing Variables
#'
#' This function adds in missing columns according to the type set in the
#' metacore object. All values in the new columns will be missing, but typed
#' correctly. If unable to recognize the type in the metacore object will return
#' a logical type.
#' @param dataset Dataset to add columns to. If all variables are present no
#'   columns will be added.
#' @param metacore metacore object that only contains the specifications for the
#'   dataset of interest.
#' @param dataset_name Optional string to specify the dataset. This is only
#'   needed if the metacore object provided hasn't already been subsetted.
#'
#' @return The given dataset with any additional columns added
#' @export
#'
#' @importFrom dplyr filter pull mutate bind_cols as_tibble
#' @importFrom purrr discard map
#' @importFrom rlang !! :=
#'
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(dplyr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' data <- read_xpt(metatools_example("adsl.xpt")) %>%
#'    select(-TRTSDT, -TRT01P, -TRT01PN)
#' add_variables(data, spec)
add_variables <- function(dataset, metacore, dataset_name = NULL){
   metacore <- make_lone_dataset(metacore, dataset_name)
   var_list <- metacore$ds_vars %>%
      filter(is.na(supp_flag) | !(supp_flag)) %>%
      pull(variable)

   to_add <- var_list %>%
      discard(~ . %in% names(dataset))
   if(length(to_add) > 0){
      n <- nrow(dataset)
      typing <- metacore$var_spec %>%
         filter(variable %in% to_add) %>%
         mutate(type_fmt = str_to_lower(type),
                out_type =
                   case_when(
                      str_detect(str_to_lower(format), "date") ~ "date",
                      type_fmt == "integer" ~ "integer",
                      type_fmt == "numeric" ~ "double",
                      type_fmt == "text" ~ "character",
                      type_fmt == "character" ~ "character",
                      type_fmt == "boolean" ~"logical",
                      type_fmt == "logical" ~"logical",
                      TRUE ~ "unknown"
                   ))

      new_cols <- map(typing$out_type, function(typ){
         out <- switch(typ,
                       "character" = rep(NA_character_, n),
                       "integer" = rep(NA_integer_, n),
                       "double" = rep(NA_real_, n),
                       "date" = as.Date(rep(NA_integer_, n)),
                       "logical" = rep(NA, n),
                       "unknown" = rep(NA, n)
         )
      })
      names(new_cols) <- typing$variable
      new_cols <- as_tibble(new_cols)

      dataset <- bind_cols(dataset, new_cols)
   }
   dataset
}
