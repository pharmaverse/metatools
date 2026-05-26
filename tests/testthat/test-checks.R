# Suppress cli output during testing
options(cli.default_handler = function(...) {})

# Load data to use across tests
load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL", verbose = "silent")
data <- haven::read_xpt(metatools_example("adsl.xpt"))

mod_ds_vars <- spec$ds_vars %>%
  mutate(core = if_else(variable %in% c("TRT01PN", "COMP8FL"), "Required", core))
spec_mod <- metacore::metacore(spec$ds_spec, mod_ds_vars, spec$var_spec, spec$value_spec, spec$derivations, spec$codelist, verbose = "silent")
spec_mod <- select_dataset(spec_mod, "ADSL", verbose = "silent")

test_that("get_bad_ct works correctly", {
  # test na_acceptable
  expect_equal(get_bad_ct(data, spec, "DCSREAS"), character(0))
  expect_equal(get_bad_ct(data, spec, "DCSREAS", TRUE), character(0))

  expect_warning(
     expect_equal(
        get_bad_ct(data, spec, "DCSREAS", FALSE),
        ""
     )
  )

  expect_warning(
     expect_equal(
        get_bad_ct(data, spec_mod, "COMP8FL"),
        ""
     )
  )

  expect_equal(get_bad_ct(data, spec_mod, "COMP8FL", TRUE), character(0))
  expect_warning(
     expect_equal(
        get_bad_ct(data, spec_mod, "COMP8FL", FALSE),
        ""
     )
  )

  data_na <- mutate(data, COMP8FL = if_else(dplyr::row_number() == 1, NA_character_, COMP8FL))
  expect_warning(
     expect_equal(
        get_bad_ct(data_na, spec_mod, "COMP8FL"),
        c(NA_character_, "")
     )
  )
})

test_that("check_ct_col works correctly", {
  # Check it works with a character col
  expect_equal(check_ct_col(data, spec, ARM), data)
  # Check it works with a numeric col
  expect_equal(check_ct_col(data, spec, TRT01PN), data)
  # Check it works when passes a string
  expect_equal(check_ct_col(data, spec, "TRT01PN"), data)

  # Check verbose parameters work (warn) and partial match (w)
  expect_silent(check_ct_col(data, spec, ARM, verbose = "warn"))
  expect_silent(check_ct_col(data, spec, ARM, verbose = "w"))

  # Check verbose warning issued when `verbose = "silent"` or partial match "s"
  expect_warning(check_ct_col(data, spec, ARM, verbose = "silent"))
  expect_warning(expect_message(
     check_ct_col(data, spec, ARM, verbose = "s")
  ))

  # Test permitted Values
  spec2 <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), verbose = "silent")
  dm <- select_dataset(spec2, "DM", verbose = "silent")
  expect_equal(check_ct_col(data, dm, ARM), data)

  # Test external dictionaries
  data2 <- tibble::tibble(AELLT = "Hello")
  ae <- select_dataset(spec2, "AE", verbose = "silent")
  expect_warning(
    check_ct_col(data2, ae, AELLT),
    "We currently don't have the ability to check against external libraries"
  )

  # Test a column that isn't in the dataset
  expect_error(
    check_ct_col(data, ae, AELLT),
    "AELLT not found in dataset. Please check and try again"
  )

  # Test NA acceptable
  expect_error(check_ct_col(data, dm, COMP8FL, FALSE))

  expect_equal(check_ct_col(data, dm, ARM, TRUE), data)
  data_w_miss <- data %>%
    mutate(TRT01PN = if_else(dplyr::row_number() == 3, NA_real_, TRT01PN))
  expect_warning(check_ct_col(data_w_miss, spec, TRT01PN, FALSE))
  expect_equal(get_bad_ct(data_w_miss, spec, TRT01PN, FALSE), NA_real_)
  expect_equal(check_ct_col(data_w_miss, spec, TRT01PN, TRUE), data_w_miss)
  ### Test with  a required column ###
  # Required without missing
  expect_equal(check_ct_col(data, spec_mod, TRT01PN), data)
  # Required with missing
  expect_warning(check_ct_col(data, spec_mod, COMP8FL))
  expect_equal(get_bad_ct(data, spec_mod, COMP8FL), "")
  expect_equal(check_ct_col(data, spec_mod, COMP8FL, TRUE), data)
})

test_that("check_ct_data works correctly", {
  # Checking error for multiple words in multiple columns
  data_multi_word <- data %>%
    mutate(
      TRT01P = case_when(
        dplyr::row_number() == 2 ~ "Hello",
        dplyr::row_number() == 3 ~ "World",
        TRUE ~ TRT01P
      ),
      TRT01A = TRT01P
    )
  expect_warning(check_ct_data(data_multi_word |> select(TRT01P), spec))

  expect_warning(check_ct_data(data |> select(COMP8FL), spec, FALSE))

  # Check data returned
  expect_equal(check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N")), data)
  expect_equal(check_ct_data(data, spec, TRUE, omit_vars = c("AGEGR2", "AGEGR2N")), data)
  expect_warning(check_ct_data(data |> select(AGEGR2), spec_mod))
  expect_equal(check_ct_data(data, spec_mod, TRUE, omit_vars = c("AGEGR2", "AGEGR2N")), data)

  # Check verbose parameter
  expect_message(
    {
      ret <- check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "message")
    },
    regexp = "All controlled terminology checks passed"
  )

  expect_silent(check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "warn"))
  expect_silent(check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "w"))

  ret <- check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "silent") |>
    expect_message("All controlled terminology checks passed") |>
    expect_warning("Argument")

  ret <- check_ct_data(data, spec, na_acceptable = c("DCSREAS", "COMP8FL", "BMIBLGR1"), verbose = "silent") |>
    expect_warning("Argument") |>
    expect_warning("Invalid controlled terminology detected")

  # Check character vector input for na_acceptable:
  expect_warning(check_ct_data(data, spec, na_acceptable = c("DCSREAS", "COMP8FL", "BMIBLGR1")))
  expect_error(check_ct_data(data, spec, 1))

  # Check omit_vars:
  expect_error(check_ct_data(data, spec, omit_vars = c("A", "B")))
  expect_warning(check_ct_data(data, spec, FALSE, omit_vars = c("DCSREAS", "COMP8FL", "BMIBLGR1")))
  expect_equal(
    check_ct_data(
      data,
      spec_mod,
      na_acceptable = NULL,
      omit_vars = c("AGEGR2", "AGEGR2N", "COMP8FL")
    ),
    data
  )
})

test_that("variable_check works correctly", {
  expect_equal(check_variables(data, spec), data)
  data_miss <- data %>% select(-1)
  expect_error(check_variables(data_miss, spec, strict = TRUE))
  expect_warning(check_variables(data_miss, spec, strict = FALSE))
  data_extra <- data %>% mutate(foo = "hello")
  expect_error(check_variables(data_extra, spec, strict = TRUE))
  expect_warning(check_variables(data_extra, spec, strict = FALSE))
  data_mis_ex <- data_extra %>% select(-1)
  expect_error(check_variables(data_mis_ex, spec, strict = TRUE))
  expect_warning(check_variables(data_mis_ex, spec, strict = FALSE))
})

test_that("check_unique_keys works as expected", {
  # check requirement for subsetted metacore object or a dataset name
  expect_error(check_unique_keys(data, metacore))
  # check missing variable keys error
  adae <- select_dataset(metacore, "ADAE", verbose = "silent")
  expect_error(check_unique_keys(data, adae))
  # check works correctly when records are unique
  adsl <- select_dataset(metacore, "ADSL", verbose = "silent")
  expect_message(check_unique_keys(data, adsl))
  # check works correctly when records are not unique
  test <- build_from_derived(adae,
    ds_list = list(
      "AE" = safetyData::sdtm_ae,
      "ADSL" = safetyData::adam_adsl
    ),
    predecessor_only = FALSE,
    keep = FALSE
  )
  expect_error(check_unique_keys(test, adae))
})
