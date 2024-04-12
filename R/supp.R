#' Build the observations for a single QNAM
#'
#' @param dataset Input dataset
#' @param qnam  QNAM value
#' @param qlabel QLABEL value
#' @param idvar IDVAR variable name (provided as a string)
#' @param qeval QEVAL value to be populated for this QNAM
#' @param qorig QORIG value to be populated for this QNAM
#'
#' @importFrom rlang sym
#' @importFrom dplyr select rename filter mutate distinct
#'
#' @return Observations structured in SUPP format
#' @export
#'
#'
build_qnam <- function(dataset, qnam, qlabel, idvar, qeval, qorig) {
   # Need QNAM as a variable
   qval <- as.symbol(qnam)

   # DM won't have an IDVAR so handle that
   if (is.na(idvar) || idvar == '') {
      dataset <- dataset %>%
         mutate(IDVARVAL = idvar)
      idvarval <- sym('IDVARVAL')

   } else {
      idvarval <- as.symbol(idvar)
   }

   dup_sup <- dataset %>%
      select(STUDYID, RDOMAIN = DOMAIN, USUBJID, !!idvarval, !!qval) %>%
      rename(IDVARVAL = !!idvarval, QVAL = !!qval) %>%
      filter(!is.na(QVAL)) %>%
      mutate(
         IDVAR = idvar,
         QNAM = qnam,
         QLABEL = qlabel,
         QORIG = qorig,
         QEVAL = qeval
      )

   out <- dup_sup %>%
      distinct(STUDYID, RDOMAIN,
               USUBJID, IDVARVAL, QNAM, .keep_all = TRUE) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM, QLABEL, QVAL,
             QORIG, QEVAL)

   test_out <- dup_sup %>%
      distinct()
   if(nrow(out) != nrow(test_out)){
      stop("The combination of STUDYID, RDOMAIN, USUBJID, IDVARVAL, QNAM is ambiguous. Consider modifying the IDVAR",
           call. = FALSE)
   }

   blank_test <- out %>%
      pull(QVAL)
   if(any(blank_test == "")){
      message(paste0("Empty QVAL rows removed for QNAM = ", unique(out$QNAM)))
      out <- out %>%
         filter(QVAL != "")
   }
   out
}



#' Make Supplemental Qualifier
#'
#' @param dataset dataset the supp will be pulled from
#' @param metacore A subsetted metacore object to get the supp information from.
#'   If not already subsetted then a `dataset_name` will need to be provided
#' @param dataset_name optional name of dataset
#'
#' @return a CDISC formatted SUPP dataset
#' @export
#'
#' @importFrom rlang as_label
#' @importFrom stringr str_remove_all
#' @importFrom dplyr filter if_else distinct
#' @importFrom purrr pmap_dfr
#'
#' @examples
#'
#' library(metacore)
#' library(safetyData)
#' library(tibble)
#' load(metacore_example("pilot_SDTM.rda"))
#' spec <- metacore %>% select_dataset("AE")
#' ae <- combine_supp(sdtm_ae, sdtm_suppae)
#' make_supp_qual(ae, spec) %>% as_tibble()
make_supp_qual <- function(dataset, metacore, dataset_name = NULL){
   #Get a single metacore object
   metacore <- make_lone_dataset(metacore, dataset_name)

   supp_vars <- metacore$ds_vars %>%
      filter(supp_flag)
   if(nrow(supp_vars) == 0){
      stop("No supplemental variables specified in metacore object. Please check your specifications",
           call. = FALSE)
   }

   supp_meta <- supp_vars %>%
      select(dataset, variable) %>%
      left_join(metacore$var_spec, by = "variable") %>%
      left_join(metacore$value_spec, by = c("dataset", "variable")) %>%
      left_join(metacore$supp,  by = c("dataset", "variable")) %>%
      select(qnam = variable, qlabel = label,
             qorig = origin, qeval = qeval,
             idvar = idvar)  %>%
      distinct() #Protection against bad specs
   #TODO Addin in checks/coercion for when combining cols of different types
   pmap_dfr(supp_meta, build_qnam, dataset=dataset) %>%
      arrange(USUBJID, QNAM, IDVARVAL)
}

#' Combine the Domain and Supplemental Qualifier
#'
#' @param dataset Domain dataset
#' @param supp Supplemental Qualifier dataset
#'
#' @return a dataset with the supp variables added to it
#' @export
#'
#' @importFrom purrr discard map reduce
#' @importFrom dplyr if_else select group_by group_split pull rename left_join
#'   any_of
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#'
#' @examples
#' library(safetyData)
#' library(tibble)
#' combine_supp(sdtm_ae, sdtm_suppae)  %>% as_tibble()
combine_supp <- function(dataset, supp){
   if(!is.data.frame(dataset) | !is.data.frame(supp)){
      stop("You must supply a domain and supplemental dataset", call. = FALSE)
   }
  if (nrow(supp) == 0) {
    warning("Zero rows in supp, returning original dataset unchanged")
    return(dataset)
  }
   supp_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
                  "QNAM", "QLABEL", "QVAL", "QORIG")
   maybe <- c("QEVAL")
   ext_supp_col <- names(supp) %>%  discard(~. %in% c(supp_cols, maybe))
   mis_supp_col <- supp_cols %>%  discard(~. %in% names(supp))
   if(length(ext_supp_col) > 0 | length(mis_supp_col) > 0){
      mess <- "Supplemental datasets need to comply with CDISC standards\n"
      ext <- if_else(length(ext_supp_col) > 0,
                     paste0("The following columns need to be removed:\n", paste0(ext_supp_col, collapse = "\n")),
                     "")
      mis <- if_else(length(mis_supp_col) > 0,
                     paste0("The following columns are missing:\n", paste0(mis_supp_col, collapse = "\n")),
                     "")
      stop(paste0(mess, ext, mis))
   }
   all_qnam <- unique(supp$QNAM)
   existing_qnam <- intersect(all_qnam, names(dataset))
   if (length(existing_qnam) > 0) {
     stop(
       "The following column(s) would be created by combine_supp(), but are already in the original dataset:\n  ",
       paste(existing_qnam, sep = ", ")
     )
   }

   # In order to prevent issues when there are multiple IDVARS we need to merge
   # each IDVAR into the domain seperately (otherwise there is problems when the
   # two IDVARS don't overlap)

   supp_wides_prep <-
     supp %>%
     select(-any_of(c("QLABEL", "QORIG", "QEVAL"))) %>% #Removing columns not for the main dataset
     rename(DOMAIN = RDOMAIN) %>%
     group_by(IDVAR, QNAM) %>% #For when there are multiple IDs
     group_split()

   supp_wides <- purrr::pmap(.l = list(supp = supp_wides_prep), .f = combine_supp_make_wide)
   ret <- reduce(.x = append(list(dataset), supp_wides), .f = combine_supp_join)
   ret
}

# Create a wide version of `supp` for merging into the source dataset.
combine_supp_make_wide <- function(supp) {
  stopifnot(length(unique(supp$IDVAR)) == 1)
  stopifnot(length(unique(supp$QNAM)) == 1)
  # Get the IDVAR value to allow for renaming of IDVARVAL
  id_var <- unique(supp$IDVAR)

  wide_x <-
    supp %>%
    pivot_wider(
      names_from = QNAM,
      values_from = QVAL
    )
  wide_x$QNAM <- unique(supp$QNAM)
  if (!is.na(id_var) && id_var != "") {
    wide_x <-
      wide_x %>%
      mutate(IDVARVAL = str_trim(as.character(IDVARVAL)))
  } else {
    wide_x$IDVARVAL <- NULL
  }
  wide_x
}

combine_supp_join <- function(dataset, supp) {
  current_idvar <- unique(supp$IDVAR)
  current_qnam <- unique(supp$QNAM)
  stopifnot(length(current_idvar) == 1)
  stopifnot(length(current_qnam) == 1)

  by <- intersect(names(supp), c("STUDYID", "DOMAIN", "USUBJID", "IDVARVAL"))
  supp_prep <- supp %>% select(-QNAM, -IDVAR)
  new_column <- setdiff(names(supp_prep), by)
  stopifnot(length(new_column) == 1)

  # Prepare IDVARVAL
  ret <- dataset
  if ("IDVARVAL" %in% by) {
    # Match the IDVARVAL column in supp
    ret$IDVARVAL <- str_trim(as.character(ret[[current_idvar]]))
  } else {
    # A dummy column that can be removed later
    ret$IDVARVAL <- FALSE
  }

  # Put the new data in
  if (new_column %in% names(dataset)) {
    # Patch the data
    mask_na_ret_before <- is.na(ret[[new_column]])
    ret_orig <- ret
    ret <- dplyr::rows_patch(x = ret, y = supp_prep, by = by)
    mask_na_ret_after <- is.na(ret[[new_column]])

    expected_na_difference <- sum(!is.na(supp_prep[[new_column]]))
    actual_na_difference <- sum(!mask_na_ret_after) - sum(!mask_na_ret_before)
    if (expected_na_difference != actual_na_difference) {
      stop(
        "An unexpected number of rows were replaced while merging QNAM ", current_qnam, " and IDVAR ", current_idvar,
        "\n  Please verify that your SUPP domain is valid SDTM with only one matched row per key column set")
    }
  } else {
    # Verify that nothing will be missed
    missing <- anti_join(supp_prep, ret, by = by)

    # Add message for when there are rows in the supp that didn't get merged
    if(nrow(missing) > 0) {
      missing_txt <-
        capture.output(
          missing %>%
            select(USUBJID, all_of(current_idvar)) %>%
            print()
        ) %>%
        paste0(collapse = "\n")
      stop(paste0("Not all rows of the Supp were merged. The following rows are missing:\n",
                  missing_txt),
           call. = FALSE)
    }

    # join the data
    ret <- left_join(ret, supp_prep, by = by)
  }
  ret
}

#' Handles the combining of datasets and supps for a single IDVAR
#'
#' @param dataset Domain dataset
#' @param supp Supplemental Qualifier dataset with a single IDVAR
#'
#' @return list of datasets
#' @noRd
#' @importFrom dplyr anti_join
#' @importFrom utils capture.output
#' @importFrom stringr str_trim
combine_supp_by_idvar <- function(dataset, supp){
   # Get the IDVAR value to allow for renaming of IDVARVAL
   id_var <- unique(supp$IDVAR)

   wide_x <- supp %>%
      pivot_wider(
         names_from = QNAM,
         values_from = QVAL) %>%
      select(-IDVAR)

   if(!is.na(id_var) && id_var != ""){
      id_var_sym <- sym(id_var)

      by <- c("STUDYID", "DOMAIN", "USUBJID", "IDVARVAL")
      wide_x <- wide_x %>%
         mutate(IDVARVAL = as.character(IDVARVAL) %>%
                   str_trim())
      #  Make a dummy IDVARVAL variable to merge on, won't effect the dataset
      dataset_chr <- dataset %>%
         mutate(IDVARVAL = as.character(!!id_var_sym) %>%
                   str_trim())

      out <- left_join(dataset_chr, wide_x,
                       by = by) %>%
         select(-IDVARVAL)
      missing <- anti_join(wide_x,dataset_chr, by = by)

      # Add message for when there are rows in the supp that didn't get merged
      if(nrow(missing) > 0) {
         missing_txt <- capture.output(missing %>%
                                          select(USUBJID, !!sym(id_var)) %>%
                                          print()) %>%
            paste0(collapse = "\n")
         stop(paste0("Not all rows of the Supp were merged. The following rows are missing:\n",
                     missing_txt),
              call. = FALSE)
      }

   } else {
      wide_x <- wide_x %>%
         select(-IDVARVAL)
      out <- left_join(dataset, wide_x,
                       by = c("STUDYID", "DOMAIN", "USUBJID"))
   }
   out
}
