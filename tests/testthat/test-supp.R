# Suppress cli output during testing
options(cli.default_handler = function(...) { })

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
      select_dataset("AE", quiet = TRUE)

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
             QORIG = "derived",
             QEVAL = "CLINICAL STUDY SPONSOR",
             QLABEL = "TREATMENT EMERGENT FLAG") %>%
      arrange(USUBJID, QNAM, IDVARVAL) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR,
             IDVARVAL, QNAM , QLABEL,QVAL,  QORIG, QEVAL) %>%
      distinct()

   #Testing normal circumstances
   expect_equal(metacore_supp, man_supp)

   # Add the supp without a idvar
   dm_spec <- select_dataset(metacore, "DM", quiet = TRUE)
   dm <- combine_supp(safetyData::sdtm_dm, safetyData::sdtm_suppdm) %>%
      as_tibble()
   dm_supp <- make_supp_qual(dm, dm_spec)
   man_dm_supp <- safetyData::sdtm_suppdm %>%
      as_tibble() %>%
      mutate(IDVAR = as.character(IDVAR),
             IDVARVAL = as.character(IDVARVAL),
             QORIG = tolower(QORIG)) %>%
      select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL)
   expect_equal(dm_supp, man_dm_supp)

   #Testing with blank rows
   supp_with_miss <- dm %>%
      dplyr::bind_rows(tibble::tibble(STUDYID = "CDISCPILOT01",
                                      DOMAIN = "DM",
                                      USUBJID = "01-701-9999",
                                      SUBJID = 9999,
                                      ITT = ""))
   expect_message(make_supp_qual(supp_with_miss, dm_spec),
                  "Empty QVAL rows removed for QNAM = ITT")

   suppressMessages(make_supp_qual(supp_with_miss, dm_spec)) %>%
      expect_equal(man_dm_supp)


   # Testing with too many datasets
   expect_error(make_supp_qual(ae, metacore))
   #Testing without supp columns specified
   metacore_old <- metacore::spec_to_metacore(metacore::metacore_example("SDTM_spec_CDISC_pilot.xlsx"), quiet = TRUE)
   ae_spec <- select_dataset(metacore_old, "AE", quiet = TRUE)
   expect_error(make_supp_qual(ae, ae_spec),
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
   attr(supp_check$AETRTEM, "label") <- 'TREATMENT EMERGENT FLAG'
   expect_equal(combo_ae, supp_check)

   ### No IDVAR and multiple QNAM
   out_test <- safetyData::sdtm_suppdm %>%
      filter(USUBJID %in% c("01-701-1015")) %>%
      select(USUBJID, QNAM, QVAL) %>%
      pivot_wider(names_from = QNAM, values_from = QVAL) %>%
      as.data.frame()
   attr(out_test$COMPLT16, "label") <- 'Completers of Week 16 Population Flag'
   attr(out_test$COMPLT24, "label") <- 'Completers of Week 24 Population Flag'
   attr(out_test$COMPLT8, "label") <- 'Completers of Week 8 Population Flag'
   attr(out_test$EFFICACY, "label") <- 'Efficacy Population Flag'
   attr(out_test$ITT, "label") <- 'Intent to Treat Population Flag'
   attr(out_test$SAFETY, "label") <- 'Safety Population Flag'

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
   attr(original, "label") <- 'PROTOCOL ENTRY CRITERIA NOT MET'
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
   attr(ae$SUPPVAR1, "label") <- "Supp Test 1"
   attr(ae$SUPPVAR2, "label") <- "Supp Test 2"
   attr(ae$SUPPVAR3, "label") <- "Supp Test 3"
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

   multi_out <- combine_supp(dataset, suppae)
   expect_equal(multi_out$SUPPVAR1, ae$SUPPVAR1)
   expect_equal(multi_out$SUPPVAR2, ae$SUPPVAR2)
   expect_equal(multi_out$SUPPVAR3, ae$SUPPVAR3)
})

test_that("combine_supp works with different IDVARVAL classes", {
    skip_if_not_installed("pharmaversesdtm")
    expect_equal(
      combine_supp(pharmaversesdtm::ae, pharmaversesdtm::suppae) %>%
         pull(AESEQ),
      pharmaversesdtm::ae %>% pull(AESEQ)
   )
})

test_that("combine_supp works with without QEVAL", {
  skip_if_not_installed("pharmaversesdtm")
  expect_silent(combine_supp(pharmaversesdtm::tr_onco, pharmaversesdtm::supptr_onco))
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
   attr(supp_check$AETRTEM, "label") <- 'TREATMENT EMERGENT FLAG'
   expect_equal(combo_ae, supp_check)
})

test_that("zero-row supp returns data unchanged with a warning (#45)", {
   expect_warning(
      result <- combine_supp(safetyData::sdtm_ae, safetyData::sdtm_suppae[0,]),
      regexp = "Zero rows in supp, returning original dataset unchanged"
   )
   expect_equal(result, safetyData::sdtm_ae)
})

test_that("multiple different IDVAR map to the same QNAM works", {
  simple_ae <-
    safetyData::sdtm_ae |>
    filter(USUBJID %in% c("01-701-1015", "01-701-1023"))
  simple_suppae <- safetyData::sdtm_suppae[c(1, 4), ]
  simple_suppae$IDVAR[2] <- "AEDTC"
  simple_suppae$IDVARVAL[2] <- "2012-09-02"
  expect_equal(
    combine_supp(simple_ae, supp = simple_suppae)$AETRTEM,
    structure(c("Y", NA, NA, NA, NA, NA, "Y"), label = 'TREATMENT EMERGENT FLAG')
  )

  # Replace the value in error
  simple_suppae <- safetyData::sdtm_suppae[c(1, 4, 7), ]
  simple_suppae$IDVAR[2] <- "AEDTC"
  simple_suppae$IDVARVAL[2] <- "2012-09-02"

  expect_error(
    combine_supp(simple_ae, supp = simple_suppae),
    regexp = "An unexpected number of rows were replaced while merging QNAM AETRTEM and IDVAR AESEQ"
  )
})

test_that("label is added in combine_supp() (#71)", {
  simple_ae <-
    safetyData::sdtm_ae |>
    filter(USUBJID %in% c("01-701-1015", "01-701-1023"))
  simple_suppae <- safetyData::sdtm_suppae[c(1, 4), ]
  labelled <- combine_supp(simple_ae, simple_suppae)
  expect_equal(attr(labelled$AETRTEM, "label"), "TREATMENT EMERGENT FLAG")
})

test_that("combine_supp() does not create an IDVARVAL column (#78)", {
  simple_ae <-
    safetyData::sdtm_ae |>
    filter(USUBJID %in% c("01-701-1015", "01-701-1023"))
  simple_suppae <- safetyData::sdtm_suppae[c(1, 4), ]
  noidvarval <- combine_supp(simple_ae, simple_suppae)
  expect_false("IDVARVAL" %in% names(noidvarval))
})
