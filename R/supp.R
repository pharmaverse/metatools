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
   if (idvar == '') {
      dataset <- dataset %>%
         mutate(IDVARVAL = '')
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
#' @param idvar The name if the ID variable. If not provided then the `IDVAR`
#'   and `IDVARVAL` columns of the supplemental dataset will be blank.
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
make_supp_qual <- function(dataset, metacore, idvar = NULL, dataset_name = NULL){
   # Convert id col to strings
   idvar_str <- as_label(enexpr(idvar)) %>%
      str_remove_all("\"") %>%
      if_else(. == "NULL", "", .)

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
      left_join(metacore$derivations, by= "derivation_id") %>%
      select(qnam = .data$variable, qlabel = .data$label,
             qorig = .data$origin, qeval = .data$derivation) %>%
      mutate(idvar = idvar_str) %>%
      distinct() #Protection against bad specs

   pmap_dfr(supp_meta, build_qnam, dataset=dataset) %>%
      arrange(.data$USUBJID, .data$QNAM, .data$IDVARVAL)
}

# make_supp_qual(ae, metacore)



