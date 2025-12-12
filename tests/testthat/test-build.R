# Suppress cli output during testing
options(cli.default_handler = function(...) {})

load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
test_that("drop_unspec_vars", {
  data <- haven::read_xpt(metatools_example("adsl.xpt")) %>%
    mutate(AGEGR2 = "DUMMY", AGEGR2N = 99, foo = "Hello", foo2 = "world")

  man_vars <- metacore$ds_vars %>%
    filter(dataset == "ADSL") %>%
    pull(variable)
  man_dat <- data %>%
    select(all_of(man_vars))
  drop_unspec_vars(data, spec) %>%
    expect_equal(man_dat)
  expect_message(drop_unspec_vars(data, spec),
    label = "The following variable(s) were dropped:\n  foo\n  foo2"
  )
})


test_that("build_from_derived", {
  ds_list <- list(DM = haven::read_xpt(metatools_example("dm.xpt")))

  expect_error(build_from_derived(spec, ds_list, keep = FALSE))
  # Vars that should be brought through
  man_vars <- spec$derivations %>%
    filter(str_detect(derivation, "^DM\\.")) %>%
    pull(derivation) %>%
    str_remove("^DM\\.") %>%
    unique() %>%
    c("TRT01P") %>%
    sort()
  build_from_derived(spec, ds_list,
    predecessor_only = FALSE,
    keep = FALSE
  ) %>%
    names() %>%
    sort() %>%
    expect_equal(man_vars)

  # Vars pulled through with old columns kept
  man_vars <- spec$derivations %>%
    filter(str_detect(derivation, "^DM\\.")) %>%
    pull(derivation) %>%
    str_remove("^DM\\.") %>%
    unique() %>%
    c(., "TRT01P") %>%
    sort()

  expect_warning(
    build_from_derived(spec, ds_list,
      predecessor_only = FALSE,
      keep = TRUE
    ) %>%
      names() %>%
      sort() %>%
      expect_equal(man_vars),
    label = paste0(
      "Setting 'keep' = TRUE has been superseded, and will be",
      " unavailable in future releases. Please consider setting",
      " 'keep' equal to 'ALL' or 'PREREQUISITE'."
    )
  )

  # Pulling through from more than one dataset
  spec2 <- metacore %>% select_dataset("ADAE", quiet = TRUE)
  adae_auto <- build_from_derived(spec2,
    ds_list = list(
      "AE" = safetyData::sdtm_ae,
      "ADSL" = safetyData::adam_adsl
    ),
    predecessor_only = FALSE,
    keep = FALSE
  )
  ae_part_vars <- spec2$derivations %>%
    filter(str_detect(derivation, "AE\\.[[:alnum:]]*$")) %>%
    pull(derivation) %>%
    str_remove("^AE\\.") %>%
    c("STUDYID", "USUBJID", .)

  ae_part <- select(safetyData::sdtm_ae, all_of(ae_part_vars))

  adsl_part_vars <- spec2$derivations %>%
    filter(str_detect(derivation, "ADSL\\.[[:alnum:]]*$")) %>%
    pull(derivation) %>%
    str_remove("^ADSL\\.")
  adsl_part <-
    select(safetyData::adam_adsl, all_of(adsl_part_vars)) |>
    rename(TRTA = TRT01A, TRTAN = TRT01AN)

  adae_man <- full_join(adsl_part, ae_part, by = c("STUDYID", "USUBJID"), multiple = "all") %>%
    select(all_of(names(adae_auto)), everything())
  expect_equal(adae_auto, adae_man)


  # Pulling through from one dataset when spec has more than one
  adae_auto_adsl_only <- build_from_derived(spec2,
    ds_list = list("ADSL" = safetyData::adam_adsl),
    predecessor_only = FALSE,
    keep = FALSE
  ) |>
    order_cols(spec2)
  adsl_man <- order_cols(adsl_part, spec2)
  expect_equal(adae_auto_adsl_only, adsl_man)

  adsl <- safetyData::adam_adsl
  ae <- safetyData::sdtm_ae
  adae_auto_unnamed <- build_from_derived(spec2,
    ds_list = list(ae, adsl),
    predecessor_only = FALSE,
    keep = FALSE
  )
  expect_equal(adae_auto, adae_man)

  expect_warning(build_from_derived(spec2,
    ds_list = list(safetyData::sdtm_ae, adsl),
    predecessor_only = FALSE,
    keep = FALSE
  ))

  # Pulling through all columns from original dataset
  adae_full <- build_from_derived(spec2,
    ds_list = list(
      "AE" = safetyData::sdtm_ae,
      "ADSL" = safetyData::adam_adsl
    ),
    predecessor_only = FALSE,
    keep = "ALL"
  )

  full_adsl_part <- safetyData::adam_adsl %>%
    mutate(TRTA = TRT01A, TRTAN = TRT01AN)

  adae_all_man <- full_join(full_adsl_part, safetyData::sdtm_ae, by = c("STUDYID", "USUBJID"), multiple = "all")

  expect_equal(adae_full, adae_all_man)

  # Pulling through columns required for future derivations
  spec3 <- metacore %>% select_dataset("ADAE", quiet = TRUE)

  adae_prereq <- build_from_derived(spec3,
    ds_list = list(
      "AE" = safetyData::sdtm_ae,
      "ADSL" = safetyData::adam_adsl
    ),
    predecessor_only = FALSE,
    keep = "PREREQUISITE"
  )

  adae_auto <- build_from_derived(spec3,
    ds_list = list(
      "AE" = safetyData::sdtm_ae,
      "ADSL" = safetyData::adam_adsl
    ),
    predecessor_only = FALSE,
    keep = "PREREQUISITE"
  )


  adae_all <- build_from_derived(spec3,
    ds_list = list(
      "AE" = safetyData::sdtm_ae,
      "ADSL" = safetyData::adam_adsl
    ),
    predecessor_only = FALSE,
    keep = "ALL"
  )

  adae_prereq_man <- adae_all %>%
    select(c(names(adae_auto)))

  expect_equal(adae_prereq, adae_prereq_man)
})


test_that("add_variables", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
  data <- haven::read_xpt(metatools_example("adsl.xpt")) %>%
    mutate(AGEGR2 = "DUMMY", AGEGR2N = 99)
  data_mis <- data %>%
    select(-TRTSDT, -TRT01P, -TRT01PN)
  # Check data when there is missing
  fx_miss <- add_variables(data_mis, spec) %>%
    select(TRTSDT, TRT01P, TRT01PN)
  man_miss <- data %>%
    mutate(
      TRTSDT = as.Date(NA_integer_),
      TRT01P = NA_character_,
      TRT01PN = NA_integer_
    ) %>%
    select(TRTSDT, TRT01P, TRT01PN)
  expect_equal(fx_miss, man_miss)
  # Check data when there isn't any missing
  expect_equal(
    add_variables(data, spec),
    data
  )
})
