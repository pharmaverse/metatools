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
      select(.data$STUDYID, RDOMAIN = .data$DOMAIN, .data$USUBJID, !!idvarval, !!qval) %>%
      rename(IDVARVAL = !!idvarval, QVAL = !!qval) %>%
      filter(!is.na(.data$QVAL)) %>%
      mutate(
         IDVAR = idvar,
         QNAM = qnam,
         QLABEL = qlabel,
         QORIG = qorig,
         QEVAL = qeval
      )

   out <- dup_sup %>%
      distinct(.data$STUDYID, .data$RDOMAIN,
               .data$USUBJID, .data$IDVARVAL, .data$QNAM, .keep_all = TRUE)
   test_out <- dup_sup %>%
      distinct()
   if(nrow(out) != nrow(test_out)){
      stop("The combination of STUDYID, RDOMAIN, USUBJID, IDVARVAL, QNAM is ambiguous. Consider modifying the IDVAR",
           call. = FALSE)
   }
   out
}



#' Make Supplemntal Qualifier
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
#' load(metacore_example("pilot_SDTM.rda"))
#' spec <- metacore %>% select_dataset("AE")
#' ae <- combine_supp(sdtm_ae, sdtm_suppae)
#' make_supp_qual(ae, spec)
make_supp_qual <- function(dataset, metacore, dataset_name = NULL){
   #Get a single metacore object
   metacore <- make_lone_dataset(metacore, dataset_name)

   supp_vars <- metacore$ds_vars %>%
      filter(.data$supp_flag)
   if(nrow(supp_vars) == 0){
      stop("No supplemental variables specified in metacore object. Please check your specifications",
           call. = FALSE)
   }

   supp_meta <- supp_vars %>%
      select(.data$dataset, .data$variable) %>%
      left_join(metacore$var_spec, by = "variable") %>%
      left_join(metacore$value_spec, by = c("dataset", "variable")) %>%
      left_join(metacore$supp,  by = c("dataset", "variable")) %>%
      select(qnam = .data$variable, qlabel = .data$label,
             qorig = .data$origin, qeval = .data$qeval,
             idvar = .data$idvar)  %>%
      distinct() #Protection against bad specs
   #TODO Addin in checks/coerision for when combining cols of different types
   pmap_dfr(supp_meta, build_qnam, dataset=dataset) %>%
      arrange(.data$USUBJID, .data$QNAM, .data$IDVARVAL)
}

#' Combine the Domain and Supplemental Qualifier
#'
#' @param dataset Domain dataset
#' @param supp Supplemental Qualifier dataset
#'
#' @return
#' @export
#'
#' @importFrom purrr discard map reduce
#' @importFrom dplyr if_else select group_by group_split pull rename left_join
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#'
#' @examples
#' library(safetyData)
#' combine_supp(sdtm_ae, sdtm_suppae)
combine_supp <- function(dataset, supp){
   if(!is.data.frame(dataset) | !is.data.frame(supp)){
      stop("You must supply a domain and supplemental dataset", call. = FALSE)
   }
   supp_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
                  "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL")
   ext_supp_col <- names(supp) %>%  discard(~. %in% supp_cols)
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
   by <- names(dataset) %>%
      discard(~ . %in% supp$QNAM) # Don't want any variables in our by statement

   # In order to prevent issues when there are multiple IDVARS we need to merge
   # each IDVAR into the domain seperately (otherwise there is problems when the
   # two IDVARS don't overlap)
   supp %>%
      select(-.data$QLABEL, -.data$QORIG, -.data$QEVAL) %>% #Removing columns not for the main dataset
      rename(DOMAIN = .data$RDOMAIN) %>%
      group_by(.data$IDVAR) %>% #For when there are multiple IDs
      group_split() %>%
      map(function(x) {
         # Get the IDVAR value to allow for renaming of IDVARVAL
         id_var <- x %>%
            pull(.data$IDVAR) %>%
            unique()
         wide_x <- x %>%
            pivot_wider(
               names_from = .data$QNAM,
               values_from = .data$QVAL) %>%
            select(-.data$IDVAR)
         if(!is.na(id_var) && id_var  != ""){
            wide_x <- wide_x %>%
               rename(!!sym(id_var) := .data$IDVARVAL) #Given there is only one ID per df we can just rename

            by <- c("STUDYID", "DOMAIN", "USUBJID", id_var)

            out <- left_join(dataset, wide_x,
                      by = by)
         } else {
            wide_x <- wide_x %>%
               select(-.data$IDVARVAL)
            out <- left_join(dataset, wide_x,
                             by = c("STUDYID", "DOMAIN", "USUBJID"))
         }
         out
      }) %>%
      reduce(full_join, by= by)

}

