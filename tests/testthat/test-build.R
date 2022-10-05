
load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL")
test_that("drop_unspec_vars", {
  data <- haven::read_xpt(metatools_example("adsl.xpt")) %>%
    mutate(foo = "Hello", foo2 = "world")

  man_vars <- metacore$ds_vars %>%
    filter(dataset == "ADSL") %>%
    pull(variable)
  man_dat <- data %>%
    select(all_of(man_vars))
  drop_unspec_vars(data, spec) %>%
    expect_equal(man_dat)
  expect_message(drop_unspec_vars(data, spec),
                 label = "The following variable(s) were dropped:\n  foo\n  foo2")

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
    ifelse(. == "ARM", "TRT01P", .) %>%
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
  build_from_derived(spec, ds_list,
    predecessor_only = FALSE,
    keep = TRUE
  ) %>%
    names() %>%
    sort() %>%
    expect_equal(man_vars)

  # Pulling through from more than one dataset
  spec2 <- metacore %>% select_dataset("ADAE")
  adae_auto <- build_from_derived(spec2,
     ds_list = list("AE" = safetyData::sdtm_ae,
                 "ADSL" = safetyData::adam_adsl),
     predecessor_only = FALSE,
     keep = FALSE
  )
  ae_part_vars <- spec2$derivations %>%
     filter(str_detect(derivation,"AE\\.[[:alnum:]]*$")) %>%
     pull(derivation) %>%
     str_remove("^AE\\.") %>%
     c("STUDYID", "USUBJID", .)

  ae_part <- select(safetyData::sdtm_ae, all_of(ae_part_vars))

  adsl_part_vars <- spec2$derivations %>%
     filter(str_detect(derivation,"ADSL\\.[[:alnum:]]*$")) %>%
     pull(derivation) %>%
     str_remove("^ADSL\\.") %>%
     c("STUDYID", "USUBJID", .)
  adsl_part <-
     select(safetyData::adam_adsl, all_of(adsl_part_vars))

  adae_man <- full_join(adsl_part, ae_part, by = c("STUDYID", "USUBJID")) %>%
     rename(TRTA = TRT01A, TRTAN = TRT01AN) %>%
     select(all_of(names(adae_auto)), everything())
  expect_equal(adae_auto,adae_man )

})



test_that("add_variables", {
   load(metacore::metacore_example("pilot_ADaM.rda"))
   spec <- metacore %>% select_dataset("ADSL")
   data <- haven::read_xpt(metatools_example("adsl.xpt"))
   data_mis <- data %>%
      select(-TRTSDT, -TRT01P, -TRT01PN)
   #Check data when there is missing
   fx_miss <- add_variables(data_mis, spec) %>%
      select(TRTSDT, TRT01P, TRT01PN)
   man_miss <- data %>%
      mutate(TRTSDT = as.Date(NA_integer_),
             TRT01P = NA_character_,
             TRT01PN = NA_integer_) %>%
      select(TRTSDT, TRT01P, TRT01PN)
   expect_equal(fx_miss, man_miss)
   #Check data when there isn't any missing
   expect_equal(add_variables(data, spec),
                data)
})
