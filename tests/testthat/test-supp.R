
test_that("build_qnam", {
   full_ae <- safetyData::sdtm_suppae %>%
      select(-QORIG, -QEVAL, -QLABEL) %>%
      pivot_wider(names_from = QNAM, values_from = QVAL) %>%
      rename(AESEQ = IDVARVAL) %>%
      select(-IDVAR) %>%
      left_join(safetyData::sdtm_ae, . , by = c("STUDYID", "USUBJID", "AESEQ"))

   supp_fx <- build_qnam(full_ae, "AETRTEM", "TREATMENT EMERGENT FLAG",
                         "AESEQ", "CLINICAL STUDY SPONSOR", "DERIVED") %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL)%>%
      arrange(USUBJID, IDVARVAL)
   ex_supp <- arrange(safetyData::sdtm_suppae, USUBJID, IDVARVAL)
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
      dplyr::slice(1) %>%
      build_qnam("AETRTEM", "TREATMENT EMERGENT FLAG",
                 "", "CLINICAL STUDY SPONSOR", "DERIVED") %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL) %>%
      arrange(USUBJID, IDVARVAL)
   ex_supp_sans_id <- arrange(safetyData::sdtm_suppae, USUBJID, IDVARVAL) %>%
      group_by(USUBJID) %>%
      dplyr::slice(1) %>%
      mutate(IDVAR = "", IDVARVAL = "")
   expect_equal(supp_sans_id, ex_supp_sans_id)

})

test_that("make_supp_qual", {
   load(metacore::metacore_example("pilot_SDTM.rda"))

   spec<- metacore %>%
      select_dataset("AE")

   # Add the mock supp variables
   ae <- combine_supp(safetyData::sdtm_ae, safetyData::sdtm_suppae)

   metacore_supp <- make_supp_qual(ae, spec) %>%
      arrange(USUBJID, QNAM, IDVARVAL) %>%
      as_tibble()

   man_supp <- ae %>%
      select(STUDYID, USUBJID, RDOMAIN = DOMAIN, IDVARVAL = AESEQ, AETRTEM) %>%
      tidyr::pivot_longer(AETRTEM, names_to = "QNAM", values_to = "QVAL") %>%
      filter(!is.na(QVAL)) %>%
      mutate(IDVAR = "AESEQ",
             QORIG = "DERIVED",
             QEVAL = "CLINICAL STUDY SPONSOR",
             QLABEL = "TREATMENT EMERGENT FLAG") %>%
      arrange(USUBJID, QNAM, IDVARVAL) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL) %>%
      distinct()
   man_supp <- purrr::map_df(man_supp, function(x){
      attr(x, "label") <-NULL
      x
   })



   #Testing normal circumstances
   expect_equal(metacore_supp, man_supp)



   # Add the supp without a idvar
   dm <- combine_supp(safetyData::sdtm_dm, safetyData::sdtm_suppdm) %>%
      as_tibble()
   dm_supp <- make_supp_qual(dm, metacore, "DM")
   man_dm_supp <- safetyData::sdtm_suppdm %>%
      as_tibble() %>%
      mutate(IDVAR = as.character(IDVAR),
             IDVARVAL = as.character(IDVARVAL)) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL)
   expect_equal(dm_supp, man_dm_supp)

   #Testing with blank rows
   supp_with_miss <- dm %>%
      dplyr::bind_rows(tibble::tibble(STUDYID = "CDISCPILOT01",
                      DOMAIN = "DM",
                      USUBJID = "01-701-9999",
                      SUBJID = 9999,
                      ITT = ""))
   expect_message(make_supp_qual(supp_with_miss, metacore, "DM"),
                  "Empty QVAL rows removed for QNAM = ITT")

   suppressMessages(make_supp_qual(supp_with_miss, metacore, "DM")) %>%
      expect_equal(man_dm_supp)



   # Testing with too many datasets
   expect_error(make_supp_qual(ae, metacore),
                "Requires either a subsetted metacore object or a dataset name")
   #Testing without supp columns specified
   metacore_old <- metacore::spec_to_metacore(metacore::metacore_example("SDTM_spec_CDISC_pilot.xlsx"), quiet = TRUE)
   expect_error(make_supp_qual(ae, metacore_old, "AE"),
                "No supplemental variables specified in metacore object. Please check your specifications")

})


test_that("combine_supp", {
   ### 1 IDVAR and 1 QNAM
   combo_ae <- combine_supp(safetyData::sdtm_ae, safetyData::sdtm_suppae) %>%
      select(USUBJID, AESEQ, AETRTEM) %>%
      distinct() %>%
      arrange(USUBJID, AESEQ)
   supp_check <- safetyData::sdtm_suppae %>%
      select(USUBJID, AESEQ = IDVARVAL, AETRTEM = QVAL) %>%
      arrange(USUBJID, AESEQ)
   expect_equal(combo_ae, supp_check)

   ### No IDVAR and multiple QNAM
   out_test <- safetyData::sdtm_suppdm %>%
      filter(USUBJID %in% c("01-701-1015")) %>%
      select(USUBJID, QNAM, QVAL) %>%
      pivot_wider(names_from = QNAM, values_from = QVAL) %>%
      as.data.frame()

   full_dm <- combine_supp(safetyData::sdtm_dm, safetyData::sdtm_suppdm) %>%
      select(USUBJID, COMPLT16:SAFETY)
   expect_equal(filter(full_dm, USUBJID == "01-701-1015"), out_test)
   # Test SUBJID that wasn't in the SUPP that all supp values are NA
   full_dm %>%
      filter(USUBJID == "01-701-1057") %>%
      select(-USUBJID) %>%
      tidyr::pivot_longer(everything())%>%
      dplyr::summarise(test = all(is.na(value))) %>%
      expect_equal(tibble::tibble(test = TRUE))

   ### Where there are only value for a small number of subjects
   mostly_miss <- combine_supp(safetyData::sdtm_ds, safetyData::sdtm_suppds)
   original <- safetyData::sdtm_suppds %>%
      arrange(USUBJID) %>%
      pull(QVAL)
   expect_equal(mostly_miss %>%
                      filter(!is.na(ENTCRIT)) %>%
                      arrange(USUBJID) %>%
                      pull(ENTCRIT),
                   original)

   ### Multiple IDVARS and multiple QNAMS
   # Add some mock supp variables
   ae <- safetyData::sdtm_ae %>%
      mutate(
         SUPPVAR1 = letters[1:nrow(safetyData::sdtm_ae)],
         SUPPVAR2 = rep(letters, 36)[1:nrow(safetyData::sdtm_ae)],
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
      dplyr::summarise(v1 = all(all.equal(SUPPVAR1.x, SUPPVAR1.y)), #Because there are NA rows
                v2 = all(all.equal(SUPPVAR2.x, SUPPVAR2.y)),
                v3 = all(SUPPVAR3.x == SUPPVAR3.y)) %>%
      tidyr::pivot_longer(everything()) %>%
      pull(value) %>%
      all()
   expect_equal(multi_out, TRUE)
})

test_that("combine_supp works with different IDVARVAL classes", {
   expect_equal(
      combine_supp(admiral.test::admiral_ae, admiral.test::admiral_suppae) %>%
      pull(AESEQ),
      admiral.test::admiral_ae %>% pull(AESEQ)
   )
})

test_that("combine_supp works with without QEVAL", {
   expect_silent(combine_supp(admiral.test::admiral_tr, admiral.test::admiral_supptr))
})

test_that("supp data that does not match the main data will raise a warning", {
   sdtm_suppae_extra <- safetyData::sdtm_suppae
   sdtm_suppae_extra$IDVARVAL[1] <- 99
   expect_error(
      combine_supp(safetyData::sdtm_ae, sdtm_suppae_extra)
   )
})

test_that("Floating point correction works", {
   fp1 = 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1
   sdtm_ae_fp <- safetyData::sdtm_ae %>%
      mutate(AESEQ = case_when(AESEQ == 1 ~ fp1,
                               TRUE ~ as.double(AESEQ)))
   # correction
   combo_ae <-combine_supp(sdtm_ae_fp, safetyData::sdtm_suppae) %>%
      select(USUBJID, AESEQ, AETRTEM) %>%
      distinct() %>%
      arrange(USUBJID, AESEQ)
   supp_check <- safetyData::sdtm_suppae %>%
      select(USUBJID, AESEQ = IDVARVAL, AETRTEM = QVAL) %>%
      arrange(USUBJID, AESEQ)
   expect_equal(combo_ae, supp_check)
   })


