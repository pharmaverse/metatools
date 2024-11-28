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
#' @param ds_list Named list of datasets that are needed to build the from. If
#'   the list is unnamed,then it will use the names of the objects.
#' @param dataset_name Optional string to specify the dataset that is being
#'   built. This is only needed if the metacore object provided hasn't already
#'   been subsetted.
#' @param predecessor_only By default `TRUE`, so only variables with the origin
#'   of 'Predecessor' will be used. If `FALSE` any derivation matching the
#'   dataset.variable will be used.
#' @param keep String to determine which columns from the original datasets
#'   should be kept
#'   - "FALSE" (default):  only columns that are also present in the ADaM
#'                            specification are kept in the output.
#'   - "ALL":              all original columns are carried through to the
#'                            ADaM, including those that have been renamed.
#'                            e.g. if DM.ARM is a predecessor to DM.TRT01P,
#'                            both ARM and TRT01P will be present as columns
#'                            in the ADaM output.
#'   - "PREREQUISITE":     columns are retained if they are required for future
#'                            derivations in the specification. Additional
#'                            prerequisite columns are identified as columns
#'                            that appear in the 'derivation' column of the
#'                            metacore object, but not as direct predecessors.
#'                            Predecessors are defined as columns where the
#'                            derivation is a 1:1 copy of a column in a source
#'                            dataset.
#'
#'                            e.g. derivation = "VS.VSTESTCD" is a predecessor,
#'                            while derivation = "Value of VS.VSSTRESN where
#'                            VS.VSTESTCD == 'Heart Rate'" contains both
#'                            VS.VSTESTCD and VS.VSSTRESN as prerequisites, and
#'                            these columns will be kept through to the ADaM.
#'
#'
#' @return dataset
#' @export
#' @importFrom stringr str_to_lower str_detect str_extract str_to_upper
#'   str_split str_match_all
#' @importFrom dplyr filter pull mutate group_by group_split inner_join select
#'   full_join bind_rows
#' @importFrom tidyr unnest
#' @importFrom purrr map reduce
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_warning cli_alert_info
#'
#' @examples
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADSL")
#' ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))
#' build_from_derived(spec, ds_list, predecessor_only = FALSE)
#'
#' # Building an ADaM (ADVS) from multiple input datasets, keeping columns
#' # needed for future transformations
#' library(metacore)
#' library(haven)
#' library(magrittr)
#' library(safetyData)
#' load(metacore_example("pilot_ADaM.rda"))
#' spec <- metacore %>% select_dataset("ADVS")
#' ds_list <- list("VS" = safetyData::sdtm_vs,"ADSL" = safetyData::adam_adsl)
#' build_from_derived(spec,
#'                    ds_list,
#'                    predecessor_only = FALSE,
#'                    keep = "PREREQUISITE"
#' )

build_from_derived <- function(metacore, ds_list, dataset_name = NULL,
                               predecessor_only = TRUE, keep = FALSE) {
   # Deprecate KEEP = TRUE
   keep <- match.arg(as.character(keep), c("TRUE", "FALSE", "ALL", "PREREQUISITE"))
   if (keep == "TRUE"){
      cli_alert_warning(paste0("Setting 'keep' = TRUE has been superseded",
      ", and will be unavailable in future releases. Please consider setting ",
      "'keep' equal to 'ALL' or 'PREREQUISITE'."))
   }
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
         stop("No predecessor variables found please check your metacore object")
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
   if(is.null(names(ds_list))){
      names(ds_list) <- deparse(substitute(ds_list)) |>
         str_remove("list\\s?\\(") |>
         str_remove("\\)s?$") |>
         str_split(",\\s?") |>
         unlist()
   }
   names(ds_list) <- names(ds_list) %>%
      str_to_lower()
   if (!all(ds_names %in% names(ds_list))) {
      unknown <- keep(names(ds_list), ~!.%in% ds_names)
      if(length(unknown) > 0){
         warning(paste0("The following dataset(s) have no predecessors and will be ignored:\n"),
                 paste0(unknown, collapse = ", "),
                 call. = FALSE)
      }
      ds_using <- discard(names(ds_list), ~. %in% unknown) |>
         str_to_upper() |>
         paste0(collapse = ", ")

      message(paste0(
         "Not all datasets provided. Only variables from ",
         ds_using,
         " will be gathered."
      ))

      # Filter out any variable that come from datasets that aren't present
      vars_w_ds <- vars_w_ds |>
         filter(ds %in% names(ds_list))

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
      map(get_variables, ds_list, keep, derirvations) %>%
      prepare_join(join_by, names(ds_list)) %>%
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
get_variables <- function(x, ds_list, keep, derivations) {
   ds_name <- unique(x$ds)
   data <- ds_list[[ds_name]]
   rename_vec <- set_names(x$col_name, x$variable)
   if (keep == "TRUE") {
      # Don't drop predecessor columns
      out <- data %>%
         select(x$col_name) %>%
         mutate(across(all_of(rename_vec)))
   } else if (keep == "FALSE") {
      # Drop predecessor columns
      out <- data %>%
         select(x$col_name) %>%
         mutate(across(all_of(rename_vec))) %>%
         select(x$variable)
   } else if (keep == "ALL") {
      # Keep all cols from original datasets
      out <- data %>%
         mutate(across(all_of(rename_vec)))
   } else if (keep == "PREREQUISITE") {
      # Keep all columns required for future derivations
      # Find all "XX.XXXXX"
      future_derivations <- derivations %>%
         select(derivation) %>%
         filter(!str_detect(derivation,"^[A-Z0-9a-z]+\\.[A-Z0-9a-z]+$"))

      prereq_vector <- str_match_all(future_derivations$derivation, "([A-Z0-9a-z]+)\\.([A-Z0-9a-z]+)")

      # Bind into matrix + remove dups
      prereq_matrix <- do.call(rbind,prereq_vector) %>%
         unique()

      # Subset to those present in current dataset
      prereq_cols <- subset(prereq_matrix, tolower(prereq_matrix[,2]) == tolower(ds_name))[,3]

      out <- data %>%
         select(c(x$col_name, all_of(prereq_cols))) %>%
         mutate(across(all_of(rename_vec))) %>%
         select(c(x$variable, all_of(prereq_cols)))
   }
   out
}

#' Internal function to remove duplicated non-key variables prior to join
#'
#' This function is used with `build_from_derived` to drop columns that would
#' cause a conflict on joining datasets, prioritising keeping columns in
#' datasets earlier on in ds_list.
#'
#' e.g. if ds_list = ("AE", "ADSL") and there is a conflicting column
#' "STUDYID", the column will be dropped from ADSL (index 2) rather than AE
#' (index 1).
#'
#' @param x List of datasets with all columns added
#' @param keys List of key values to join on
#'
#' @return datasets
#' @noRd
prepare_join <- function(x, keys, ds_names) {
   out <- list(x[[1]])

   if (length(x) > 1){
      for (i in 2:length(x)){
         # Drop non-key cols present in each previous dataset in order
         drop_cols <- c()

         for (j in 1:(i-1)){
            conflicting_cols <- keep(names(x[[j]]), function(col) !(col %in% keys)) %>%
               intersect(colnames(x[[i]]))
            drop_cols <- c(drop_cols, conflicting_cols)

            if(length(conflicting_cols) > 0){
               cli_alert_info(paste0("Dropping column(s) from ", ds_names[[i]],
                  " due to conflict with ",ds_names[[j]],": ", conflicting_cols,"."))
            }
         }

         out[[i]] <- x[[i]] %>%
            select(-any_of(drop_cols))
      }
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
