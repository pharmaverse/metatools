# Suppress cli output during testing
options(cli.default_handler = function(...) {})

test_that("build_qnam", {
  full_ae <- safetyData::sdtm_suppae %>%
    select(-QORIG, -QEVAL, -QLABEL) %>%
    pivot_wider(names_from = QNAM, values_from = QVAL) %>%
    rename(AESEQ = IDVARVAL) %>%
    select(-IDVAR) %>%
    left_join(safetyData::sdtm_ae, ., by = c("STUDYID", "USUBJID", "AESEQ"))

  supp_fx <- build_qnam(
    full_ae, "AETRTEM", "TREATMENT EMERGENT FLAG",
    "AESEQ", "CLINICAL STUDY SPONSOR", "DERIVED"
  ) %>%
    select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL) %>%
    arrange(USUBJID, IDVARVAL)
  ex_supp <- arrange(safetyData::sdtm_suppae, USUBJID, IDVARVAL)
  # Test standard example
  expect_equal(supp_fx, ex_supp)
  # Test without IDVAR making ambiguous output
  expect_error(
    build_qnam(
      full_ae, "AETRTEM", "TREATMENT EMERGENT FLAG",
      "", "CLINICAL STUDY SPONSOR", "DERIVED"
    ),
    "The combination of STUDYID, RDOMAIN, USUBJID, IDVARVAL, QNAM is ambiguous. Consider modifying the IDVAR"
  )
  # Test without IDVAR
  supp_sans_id <- full_ae %>%
    group_by(USUBJID) %>%
    arrange(AESEQ) %>%
    dplyr::slice(1) %>%
    build_qnam(
      "AETRTEM", "TREATMENT EMERGENT FLAG",
      "", "CLINICAL STUDY SPONSOR", "DERIVED"
    ) %>%
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

  spec <- metacore %>%
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
    mutate(
      IDVAR = "AESEQ",
      QORIG = "derived",
      QEVAL = "CLINICAL STUDY SPONSOR",
      QLABEL = "TREATMENT EMERGENT FLAG"
    ) %>%
    arrange(USUBJID, QNAM, IDVARVAL) %>%
    select(
      STUDYID, RDOMAIN, USUBJID, IDVAR,
      IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL
    ) %>%
    distinct()

  # Testing normal circumstances
  expect_equal(metacore_supp, man_supp)

  # Add the supp without a idvar
  dm_spec <- select_dataset(metacore, "DM", quiet = TRUE)
  dm <- combine_supp(safetyData::sdtm_dm, safetyData::sdtm_suppdm) %>%
    as_tibble()
  dm_supp <- make_supp_qual(dm, dm_spec)
  man_dm_supp <- safetyData::sdtm_suppdm %>%
    as_tibble() %>%
    mutate(
      IDVAR = as.character(IDVAR),
      IDVARVAL = as.character(IDVARVAL),
      QORIG = tolower(QORIG)
    ) %>%
    select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL)
  expect_equal(dm_supp, man_dm_supp)

  # Testing with blank rows
  supp_with_miss <- dm %>%
    dplyr::bind_rows(tibble::tibble(
      STUDYID = "CDISCPILOT01",
      DOMAIN = "DM",
      USUBJID = "01-701-9999",
      SUBJID = 9999,
      ITT = ""
    ))
  expect_message(
    make_supp_qual(supp_with_miss, dm_spec),
    "Empty QVAL rows removed for QNAM = ITT"
  )

  suppressMessages(make_supp_qual(supp_with_miss, dm_spec)) %>%
    expect_equal(man_dm_supp)


  # Testing with too many datasets
  expect_error(make_supp_qual(ae, metacore))
  # Testing without supp columns specified
  metacore_old <- metacore::spec_to_metacore(metacore::metacore_example("SDTM_spec_CDISC_pilot.xlsx"), quiet = TRUE)
  ae_spec <- select_dataset(metacore_old, "AE", quiet = TRUE)
  expect_error(
    make_supp_qual(ae, ae_spec),
    "No supplemental variables specified in metacore object. Please check your specifications"
  )
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
  attr(supp_check$AETRTEM, "label") <- "TREATMENT EMERGENT FLAG"
  expect_equal(combo_ae, supp_check)

  ### No IDVAR and multiple QNAM
  out_test <- safetyData::sdtm_suppdm %>%
    filter(USUBJID %in% c("01-701-1015")) %>%
    select(USUBJID, QNAM, QVAL) %>%
    pivot_wider(names_from = QNAM, values_from = QVAL) %>%
    as.data.frame()
  attr(out_test$COMPLT16, "label") <- "Completers of Week 16 Population Flag"
  attr(out_test$COMPLT24, "label") <- "Completers of Week 24 Population Flag"
  attr(out_test$COMPLT8, "label") <- "Completers of Week 8 Population Flag"
  attr(out_test$EFFICACY, "label") <- "Efficacy Population Flag"
  attr(out_test$ITT, "label") <- "Intent to Treat Population Flag"
  attr(out_test$SAFETY, "label") <- "Safety Population Flag"

  full_dm <- combine_supp(safetyData::sdtm_dm, safetyData::sdtm_suppdm) %>%
    select(USUBJID, COMPLT16:SAFETY)
  expect_equal(filter(full_dm, USUBJID == "01-701-1015"), out_test)
  # Test SUBJID that wasn't in the SUPP that all supp values are NA
  full_dm %>%
    filter(USUBJID == "01-701-1057") %>%
    select(-USUBJID) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::summarise(test = all(is.na(value))) %>%
    expect_equal(tibble::tibble(test = TRUE))

  ### Where there are only value for a small number of subjects
  mostly_miss <- combine_supp(safetyData::sdtm_ds, safetyData::sdtm_suppds)
  original <- safetyData::sdtm_suppds %>%
    arrange(USUBJID) %>%
    pull(QVAL)
  attr(original, "label") <- "PROTOCOL ENTRY CRITERIA NOT MET"
  expect_equal(
    mostly_miss %>%
      filter(!is.na(ENTCRIT)) %>%
      arrange(USUBJID) %>%
      pull(ENTCRIT),
    original
  )

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
  suppae <- pmap_dfr(supp_meta, build_qnam, dataset = ae) %>%
    arrange(USUBJID, QNAM, IDVARVAL)

  dataset <- ae %>%
    select(-starts_with("SUPP"))
  supp <- suppae

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

test_that("supp data that does not match the main data will raise a warning but return a dataset", {
  sdtm_suppae_extra <- safetyData::sdtm_suppae
  sdtm_suppae_extra$IDVARVAL[1] <- 99

  expect_warning(
    out <- combine_supp(safetyData::sdtm_ae, sdtm_suppae_extra),
    "Not all rows of SUPP were merged"
  )
  expect_s3_class(out, "data.frame")
})


test_that("Floating point correction works", {
  fp1 <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1
  sdtm_ae_fp <- safetyData::sdtm_ae %>%
    mutate(AESEQ = case_when(
      AESEQ == 1 ~ fp1,
      TRUE ~ as.double(AESEQ)
    ))
  # correction
  combo_ae <- combine_supp(sdtm_ae_fp, safetyData::sdtm_suppae) %>%
    select(USUBJID, AESEQ, AETRTEM) %>%
    distinct() %>%
    arrange(USUBJID, AESEQ)
  supp_check <- safetyData::sdtm_suppae %>%
    select(USUBJID, AESEQ = IDVARVAL, AETRTEM = QVAL) %>%
    arrange(USUBJID, AESEQ)
  attr(supp_check$AETRTEM, "label") <- "TREATMENT EMERGENT FLAG"
  expect_equal(combo_ae, supp_check)
})

test_that("zero-row supp returns data unchanged with a warning (#45)", {
  expect_warning(
    result <- combine_supp(safetyData::sdtm_ae, safetyData::sdtm_suppae[0, ]),
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
    structure(c("Y", NA, NA, NA, NA, NA, "Y"), label = "TREATMENT EMERGENT FLAG")
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

test_that("make_supp_qual handles deprecated dataset_name parameter", {
  load(metacore::metacore_example("pilot_SDTM.rda"))
  ae <- combine_supp(safetyData::sdtm_ae, safetyData::sdtm_suppae)

  # Test that dataset_name is deprecated and shows guidance
  suppressWarnings({
    result <- make_supp_qual(ae, metacore, dataset_name = "AE")
  })

  expect_s3_class(result, "data.frame")
})

test_that("combine_supp handles QNAM not in dataset columns", {
  simple_ae <- safetyData::sdtm_ae[1:5, ]
  simple_suppae <- safetyData::sdtm_suppae[1, ]
  simple_suppae$QNAM <- "NEWCOL" # A new column to add

  # Should successfully add the new column
  result <- combine_supp(simple_ae, simple_suppae)
  expect_true("NEWCOL" %in% names(result))
})

test_that("combine_supp errors when QNAM already exists in dataset", {
  simple_ae <- safetyData::sdtm_ae[1:5, ]
  simple_ae$AETRTEM <- "existing" # Add column that matches QNAM
  simple_suppae <- safetyData::sdtm_suppae[1, ]

  expect_error(
    combine_supp(simple_ae, simple_suppae),
    "already in the original dataset"
  )
})

test_that("combine_supp handles IDVAR not in dataset", {
  simple_ae <- safetyData::sdtm_ae[1:5, ]
  simple_suppae <- safetyData::sdtm_suppae[1, ]
  simple_suppae$IDVAR <- "FAKEIDVAR" # IDVAR that doesn't exist

  expect_error(
    combine_supp(simple_ae, simple_suppae),
    "replacement has 0 rows"
  )
})

test_that("combine_supp_by_idvar detects conflicting replacements across IDVARs", {
  simple_ae <- safetyData::sdtm_ae %>%
    filter(USUBJID %in% c("01-701-1015", "01-701-1023")) %>%
    mutate(NEWID = dplyr::row_number())

  # Create supp with same QNAM but different IDVARs that would cause conflicts
  suppae_conflict <- bind_rows(
    data.frame(
      STUDYID = "CDISCPILOT01",
      RDOMAIN = "AE",
      USUBJID = "01-701-1015",
      IDVAR = "AESEQ",
      IDVARVAL = "1",
      QNAM = "TESTVAR",
      QLABEL = "Test Variable",
      QVAL = "ValueA",
      QORIG = "CRF",
      QEVAL = "",
      stringsAsFactors = FALSE
    ),
    data.frame(
      STUDYID = "CDISCPILOT01",
      RDOMAIN = "AE",
      USUBJID = "01-701-1015",
      IDVAR = "NEWID",
      IDVARVAL = "1",
      QNAM = "TESTVAR",
      QLABEL = "Test Variable",
      QVAL = "ValueB", # Different value for same subject/QNAM
      QORIG = "CRF",
      QEVAL = "",
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    combine_supp(simple_ae, suppae_conflict),
    "unexpected number of rows"
  )
})

test_that("combine_supp: all SUPP rows merge cleanly (#98)", {
  pc <- tibble::tibble(
    STUDYID = "STUDY123",
    DOMAIN = "PC",
    USUBJID = c("01-001", "01-002", "01-003"),
    PCSEQ = c(1, 2, 3),
    PCTESTCD = "CONC",
    PCTEST = "Concentration",
    PCORRES = c("5.1", "7.3", "4.8"),
    PCORRESU = "ng/mL",
    PCSTRESC = c("5.1", "7.3", "4.8"),
    PCSTRESN = c(5.1, 7.3, 4.8),
    PCSTRESU = "ng/mL",
    PCPOS = "PLASMA",
    PCDTC = c("2025-07-15T08:00", "2025-07-15T09:00", "2025-07-15T10:00"),
    VISITNUM = 1,
    VISIT = "Visit 1",
    ARM = "Drug A",
    ACTARM = "Drug A"
  )

  supppc <- tibble::tibble(
    STUDYID  = "STUDY123",
    RDOMAIN  = "PC",
    USUBJID  = c("01-001", "01-002", "01-003"),
    IDVAR    = "PCSEQ",
    IDVARVAL = c(1, 2, 3),
    QNAM     = "PCREASND",
    QLABEL   = "Reason Not Done",
    QVAL     = c("NA", "NA", "NA"),
    QORIG    = "SPONSOR",
    QEVAL    = "INVESTIGATOR"
  )
  expect_no_warning(
    out <- combine_supp(pc, supppc)
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(pc))
  expect_true("PCREASND" %in% names(out))
  expect_equal(
    unname(as.character(out$PCREASND)),
    c("NA", "NA", "NA")
  )
})

test_that("combine_supp: extra SUPP rows that do not match core raise a warning but return a dataset(#98)", {
  pc <- tibble::tibble(
    STUDYID = "STUDY123",
    DOMAIN = "PC",
    USUBJID = c("01-001", "01-002", "01-003"),
    PCSEQ = c(1, 2, 3),
    PCTESTCD = "CONC",
    PCTEST = "Concentration",
    PCORRES = c("5.1", "7.3", "4.8"),
    PCORRESU = "ng/mL",
    PCSTRESC = c("5.1", "7.3", "4.8"),
    PCSTRESN = c(5.1, 7.3, 4.8),
    PCSTRESU = "ng/mL",
    PCPOS = "PLASMA",
    PCDTC = c("2025-07-15T08:00", "2025-07-15T09:00", "2025-07-15T10:00"),
    VISITNUM = 1,
    VISIT = "Visit 1",
    ARM = "Drug A",
    ACTARM = "Drug A"
  )

  supppc <- tibble::tibble(
    STUDYID  = "STUDY123",
    RDOMAIN  = "PC",
    USUBJID  = c("01-001", "01-002", "01-003"),
    IDVAR    = "PCSEQ",
    IDVARVAL = c(1, 2, 3),
    QNAM     = "PCREASND",
    QLABEL   = "Reason Not Done",
    QVAL     = c("NA", "NA", "NA"),
    QORIG    = "SPONSOR",
    QEVAL    = "INVESTIGATOR"
  )
  supppc_extra <- dplyr::bind_rows(
    supppc,
    dplyr::mutate(supppc[3, ], IDVARVAL = 99),
    dplyr::mutate(supppc[3, ], IDVARVAL = 101)
  )

  expect_warning(
    out <- combine_supp(pc, supppc_extra),
    "Not all rows of SUPP were merged"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(pc))
  expect_true("PCREASND" %in% names(out))
  expect_equal(
    unname(as.character(out$PCREASND)),
    c("NA", "NA", "NA")
  )
  expect_false(any(out$PCSEQ %in% c(99, 101)))
})

test_that("build_qnam verbose parameter", {
  # Create simple test data with a column that will be used as QNAM
  ae <- safetyData::sdtm_ae %>%
    head(10) %>%
    mutate(TESTVAR = c("", "", "Y", "Y", "N", "", "Y", "N", "Y", "")) # Some empty strings

  # Test verbose = "message" (default) - should show message about empty QVAL
  expect_message(
    build_qnam(
      dataset = ae,
      qnam = "TESTVAR",
      qlabel = "Test Variable",
      idvar = "AESEQ",
      qeval = "INVESTIGATOR",
      qorig = "CRF",
      verbose = "message"
    ),
    "Empty QVAL rows removed for QNAM = TESTVAR"
  )

  # Test verbose = "warn" - suppress messages
  expect_silent(
    result_warn <- build_qnam(
      dataset = ae,
      qnam = "TESTVAR",
      qlabel = "Test Variable",
      idvar = "AESEQ",
      qeval = "INVESTIGATOR",
      qorig = "CRF",
      verbose = "warn"
    )
  )

  # Test verbose = "silent" - suppress all output
  expect_silent(
    result_silent <- build_qnam(
      dataset = ae,
      qnam = "TESTVAR",
      qlabel = "Test Variable",
      idvar = "AESEQ",
      qeval = "INVESTIGATOR",
      qorig = "CRF",
      verbose = "silent"
    )
  )

  # Verify all verbose levels return same result
  result_message <- suppressMessages(
    build_qnam(
      dataset = ae,
      qnam = "TESTVAR",
      qlabel = "Test Variable",
      idvar = "AESEQ",
      qeval = "INVESTIGATOR",
      qorig = "CRF",
      verbose = "message"
    )
  )

  expect_equal(result_message, result_warn)
  expect_equal(result_message, result_silent)

  # Verify empty strings were actually removed
  expect_false("" %in% result_message$QVAL)

  # Test invalid verbose value
  expect_error(
    build_qnam(
      dataset = ae,
      qnam = "TESTVAR",
      qlabel = "Test Variable",
      idvar = "AESEQ",
      qeval = "INVESTIGATOR",
      qorig = "CRF",
      verbose = "invalid"
    ),
    "should be one of: message, warn, silent"
  )
})
