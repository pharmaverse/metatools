library(safetyData)
library(dplyr)
library(tidyr)

test_that("build_qnam", {
   full_ae <- sdtm_suppae %>%
      select(-QORIG, -QEVAL, -QLABEL) %>%
      pivot_wider(names_from = QNAM, values_from = QVAL) %>%
      rename(AESEQ = IDVARVAL) %>%
      select(-IDVAR) %>%
      left_join(sdtm_ae, . , by = c("STUDYID", "USUBJID", "AESEQ"))

   supp_fx <- build_qnam(full_ae, "AETRTEM", "TREATMENT EMERGENT FLAG",
                         "AESEQ", "CLINICAL STUDY SPONSOR", "DERIVED") %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
              IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL) %>%
      arrange(USUBJID, IDVARVAL)
   ex_supp <- arrange(sdtm_suppae, USUBJID, IDVARVAL)
   # Test standard example
   expect_equal(supp_fx, ex_supp)
   # Test without IDVAR making ambiguous output
   expect_error(build_qnam(full_ae, "AETRTEM", "TREATMENT EMERGENT FLAG",
              "", "CLINICAL STUDY SPONSOR", "DERIVED"),
              "The combination of STUDYID, RDOMAIN, USUBJID, IDVARVAL, QNAM is ambiguous. Consider modifying the IDVAR")
   # Test without IDVAR
   supp_sans_id <- full_ae %>%
      group_by(USUBJID) %>%
      arrange(AESEQ) %>%
      slice(1) %>%
      build_qnam("AETRTEM", "TREATMENT EMERGENT FLAG",
                 "", "CLINICAL STUDY SPONSOR", "DERIVED") %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL) %>%
      arrange(USUBJID, IDVARVAL)
   ex_supp_sans_id <- arrange(sdtm_suppae, USUBJID, IDVARVAL) %>%
      group_by(USUBJID) %>%
      slice(1) %>%
      mutate(IDVAR = "", IDVARVAL = "")
   expect_equal(supp_sans_id, ex_supp_sans_id)

})

test_that("make_supp_qual", {
   metacore_old <- define_to_metacore(metacore_example("SDTM_define.xml"), quiet = TRUE)

   ds_vars <- tribble(
      ~dataset, ~variable ,   ~supp_flag,
      "AE",      "SUPPVAR1",   TRUE,
      "AE",      "SUPPVAR2",   TRUE
   )

   ds_vars <- metacore_old$ds_vars %>%
      bind_rows(ds_vars)
   var_spec <- tribble(
      ~variable , ~label,
      "SUPPVAR1", "Supp Test 1",
      "SUPPVAR2","Supp Test 2",
   ) %>%
      bind_rows(metacore_old$var_spec, .)

   value_spec <-
      tribble(
         ~dataset, ~variable ,   ~origin, ~derivation_id,
         "AE",      "SUPPVAR1",   "CRF",  "AE.SUPP",
         "AE",      "SUPPVAR2",   "CRF", "AE.SUPP"
      ) %>%
      bind_rows(metacore_old$value_spec, . )

   derivation <-
      tribble(
         ~derivation_id, ~derivation,
         "AE.SUPP",       "Investigator"
      ) %>%
      bind_rows(metacore_old$derivations, . )

   metacore <- metacore(metacore_old$ds_spec, ds_vars, var_spec, value_spec, derivation, metacore_old$codelist)
   spec<- metacore %>%
      select_dataset("AE")

   # Add some mock supp variables
   ae <- sdtm_ae %>%
      mutate(
         SUPPVAR1 = words[1:nrow(ae)],
         SUPPVAR2 = rep(letters, 36)[1:nrow(ae)]
      )

   metacore_supp <- make_supp_qual(ae, spec, AESEQ) %>%
      arrange(USUBJID, QNAM, IDVARVAL) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL)
   man_supp <- ae %>%
      select(STUDYID, USUBJID, RDOMAIN = DOMAIN, IDVARVAL = AESEQ, SUPPVAR1, SUPPVAR2) %>%
      pivot_longer(starts_with("SUPP"), names_to = "QNAM", values_to = "QVAL") %>%
      filter(!is.na(QVAL)) %>%
      mutate(IDVAR = "AESEQ",
             QORIG = "CRF",
             QEVAL = "Investigator",
             QLABEL = if_else(QNAM == "SUPPVAR1", "Supp Test 1", "Supp Test 2")) %>%
      arrange(USUBJID, QNAM, IDVARVAL) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL) %>%
      distinct()
   man_supp <- map_df(man_supp, function(x){
      attr(x, "label") <-NULL
      x
   })
   #Testing normal circumstances
   expect_equal(metacore_supp, man_supp)
   #Testing with quotes
   metacore_supp <- make_supp_qual(ae, spec, "AESEQ") %>%
      arrange(USUBJID, QNAM, IDVARVAL) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL)
   expect_equal(metacore_supp, man_supp)
   # Testing with too many datasets
   expect_error(make_supp_qual(ae, metacore_old, "AESEQ"),
                "Requires either a subsetted metacore object or a dataset name")
   #Testing without supp columns specified
   expect_error(make_supp_qual(ae, metacore_old, "AESEQ", "AE"),
                "No supplemental variables specified in metacore object. Please check your specifications")

})








