library(safetyData)
library(dplyr)
library(tidyr)
library(metacore)
library(stringr)
library(purrr)

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

   metacore <- suppressWarnings(
      metacore(metacore_old$ds_spec, ds_vars, var_spec, value_spec, derivation, metacore_old$codelist))
   spec<- metacore %>%
      select_dataset("AE")

   # Add some mock supp variables
   ae <- sdtm_ae %>%
      mutate(
         SUPPVAR1 = words[1:nrow(sdtm_ae)],
         SUPPVAR2 = rep(letters, 36)[1:nrow(sdtm_ae)]
      ) %>%
      as_tibble()

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


test_that("combine_supp", {
   ### 1 IDVAR and 1 QNAM
   combo_ae <- combine_supp(sdtm_ae, sdtm_suppae) %>%
      select(USUBJID, AESEQ, AETRTEM) %>%
      distinct() %>%
      arrange(USUBJID, AESEQ)
   supp_check <- sdtm_suppae %>%
      select(USUBJID, AESEQ = IDVARVAL, AETRTEM = QVAL) %>%
      arrange(USUBJID, AESEQ)
   expect_equal(combo_ae, supp_check)

   ### No IDVAR and multiple QNAM
   out_test <- sdtm_suppdm %>%
      filter(USUBJID %in% c("01-701-1015")) %>%
      select(USUBJID, QNAM, QVAL) %>%
      pivot_wider(names_from = QNAM, values_from = QVAL) %>%
      as.data.frame()

   full_dm <- combine_supp(sdtm_dm, sdtm_suppdm) %>%
      select(USUBJID, COMPLT16:SAFETY)
   expect_equal(filter(full_dm, USUBJID == "01-701-1015"), out_test)
   # Test SUBJID that wasn't in the SUPP that all supp values are NA
   full_dm %>%
      filter(USUBJID == "01-701-1057") %>%
      select(-USUBJID) %>%
      pivot_longer(everything())%>%
      summarise(test = all(is.na(value))) %>%
      expect_equal(tibble(test = TRUE))

   ### Where there are only value for a small number of subjects
   mostly_miss <- combine_supp(sdtm_ds, sdtm_suppds)
   original <- sdtm_suppds %>%
      arrange(USUBJID) %>%
      pull(QVAL)
   expect_equal(mostly_miss %>%
                      filter(!is.na(ENTCRIT)) %>%
                      arrange(USUBJID) %>%
                      pull(ENTCRIT),
                   original)

   ### Multiple IDVARS and multiple QNAMS
   # Add some mock supp variables
   ae <- sdtm_ae %>%
      mutate(
         SUPPVAR1 = words[1:nrow(sdtm_ae)],
         SUPPVAR2 = rep(letters, 36)[1:nrow(sdtm_ae)],
         SUPPVAR3 = USUBJID,
         IDVAR = as.numeric(str_extract(USUBJID, "\\d{3}$"))
      )
   ### Mock up a metadata necessary to make the SUPP
   supp_meta <- tibble::tribble(
      ~qnam, ~qlabel, ~idvar, ~qeval, ~qorig,
      "SUPPVAR1", "Supp Test 1", "AESEQ", "Investigator", "CRF",
      "SUPPVAR2", "Supp Test 2", "AESEQ", "Investigator", "CRF",
      "SUPPVAR3", "Supp Test 3", "IDVAR", "Investigator", "CRF",
   )

   ### Wrap and map
   suppae <- pmap_dfr(supp_meta, build_qnam, dataset=ae) %>%
      arrange(USUBJID, QNAM, IDVARVAL)

   dataset = ae %>%
      select(-starts_with("SUPP"))
   supp = suppae
   multi_out <- combine_supp(ae, suppae) %>%
      summarise(v1 = all(all.equal(SUPPVAR1.x, SUPPVAR1.y)), #Because there are NA rows
                v2 = all(all.equal(SUPPVAR2.x, SUPPVAR2.y)),
                v3 = all(SUPPVAR3.x == SUPPVAR3.y)) %>%
      pivot_longer(everything()) %>%
      pull(value) %>%
      all()
   expect_equal(multi_out, TRUE)
})


