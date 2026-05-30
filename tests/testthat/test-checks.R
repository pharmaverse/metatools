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
   # Dateset not provided
   expect_error(get_bad_ct(character(0), spec), "must be a data frame")

   # Test no var provided
   expect_error(get_bad_ct(data, spec), "must be provided as a string or bare column name")

  # Test na_acceptable with valid CT
  expect_equal(get_bad_ct(data, spec, "DCSREAS"), character(0))
  expect_equal(get_bad_ct(data, spec, "DCSREAS", TRUE), character(0))

  # Test na_acceptable with invalid CT
  get_bad_ct(data, spec_mod, "COMP8FL", TRUE) |>
     expect_equal(character(0))

  get_bad_ct(data, spec_mod, "COMP8FL", FALSE) |>
     expect_equal("") |>
     expect_warning("Invalid controlled terminology detected")

  # Test empty string not permitted
  get_bad_ct(data, spec, "DCSREAS", FALSE) |>
     expect_equal("") |>
     expect_warning("Invalid controlled terminology detected")

  get_bad_ct(data, spec_mod, "COMP8FL") |>
     expect_equal("") |>
     expect_warning("Invalid controlled terminology detected")

  # Check more than one value returned
  data_na <- mutate(data, COMP8FL = if_else(dplyr::row_number() == 1, NA_character_, COMP8FL))
  get_bad_ct(data_na, spec_mod, "COMP8FL") |>
     expect_equal(c(NA_character_, "")) |>
     expect_warning("Invalid controlled terminology detected")

  # Check bad ct can be retrieved for VLM
  adex_spec <- select_dataset(vlm_spec, "ADEX", verbose = "silent")
  get_bad_ct(adex, adex_spec, "AVALCAT1") |>
     expect_type("list") |>
     expect_warning("Invalid controlled terminology detected")

  # Test .internal skips string validation (returns error as var not resolved to string)
  get_bad_ct(data, spec, ARM, .internal = TRUE) |>
     expect_error("object 'ARM' not found")
})


test_that("check_ct_col works correctly", {
   # Check it works with a character col (bare name)
   check_ct_col(data, spec, ARM) |>
      expect_equal(data) |>
      expect_message("Controlled terminology checks passed")

   # Check it works with a character col (string)
   check_ct_col(data, spec, "TRT01PN") |>
      expect_equal(data) |>
      expect_message("Controlled terminology checks passed")

   # Check it works with a numeric col
   check_ct_col(data, spec, TRT01PN) |>
      expect_equal(data) |>
      expect_message("Controlled terminology checks passed")

   # Check verbose parameters work (warn) and partial match (w)
   expect_silent(check_ct_col(data, spec, ARM, verbose = "warn"))
   expect_silent(check_ct_col(data, spec, ARM, verbose = "w"))

   # Check verbose warning issued when `verbose = "silent"` or partial match "s"
   check_ct_col(data, spec, ARM, verbose = "silent") |>
      expect_warning("Must be one of") |>
      expect_message("Controlled terminology checks passed")

   check_ct_col(data, spec, ARM, verbose = "s") |>
      expect_warning("Must be one of") |>
      expect_message("Controlled terminology checks passed")

   # Test external dictionaries
   spec2 <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), verbose = "silent")
   data2 <- tibble::tibble(AELLT = "Hello")
   ae <- select_dataset(spec2, "AE", verbose = "silent")
   check_ct_col(data2, ae, AELLT) |>
      expect_warning("We currently don't have the ability to check against external libraries")

   # Test a column that isn't in the dataset
   check_ct_col(data, ae, INVALID) |>
      expect_error("not found in dataset")

   # Test NA acceptable (empty string)
   invisible(check_ct_col(data, spec_mod, "COMP8FL", TRUE)) |>
      expect_message("Controlled terminology checks passed")

   invisible(check_ct_col(data, spec_mod, "COMP8FL", FALSE)) |>
      expect_warning("Values not permitted: \"\"")

   invisible(check_ct_col(data, spec_mod, "COMP8FL")) |>
      expect_warning("Values not permitted: \"\"") # use metadata from ds_vars$core

   # Test NA acceptable (NA)
  data_w_miss <- mutate(data, TRT01PN = if_else(dplyr::row_number() == 3, NA_real_, TRT01PN))
  invisible(check_ct_col(data_w_miss, spec, TRT01PN, FALSE)) |>
     expect_warning("Values not permitted: NA")

  # Test returns dataframe
  check_ct_col(data_w_miss, spec, TRT01PN, TRUE) |>
     expect_identical(data_w_miss) |>
     expect_message("Controlled terminology checks passed")

  # Test internal call returns TRUE/FALSE
  # .internal check must be performed with string variable as conversion is skipped
  check_ct_col(data, spec, "ARM", .internal = TRUE) |>
     expect_true()

  check_ct_col(data, spec_mod, "COMP8FL", FALSE, .internal = TRUE) |>
     expect_false() |>
     expect_warning("Invalid controlled terminology detected")

  # Test .internal skips string validation (returns error as var not resolved to string)
  check_ct_col(data, spec, ARM, .internal = TRUE) |>
     expect_error("object 'ARM' not found")
})


test_that("check_ct_data works correctly", {
  # Checking error for multiple words in a column
  data_multi_word <- data %>%
    mutate(
      TRT01P = case_when(
        dplyr::row_number() == 2 ~ "Hello",
        dplyr::row_number() == 3 ~ "World",
        TRUE ~ TRT01P
      ),
      TRT01A = TRT01P
    )

  check_ct_data(data_multi_word |> select(TRT01P), spec) |>
     expect_warning(regexp = "Values not permitted.*Hello.*World")

  # Checking error for multiple words in multiple columns
  data_multi_word <- mutate(data_multi_word, TRT01A = "Invalid")
  check_ct_data(data_multi_word |> select(TRT01P, TRT01A), spec) |>
     expect_warning(regexp = "Values not permitted.*Hello.*World") |>
     expect_warning(regexp = "Values not permitted.*Invalid")

  # Check data returned (with omit vars)
  check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N")) |>
     expect_equal(data) |>
     expect_message("All controlled terminology checks passed")

  # Check data returned (no omit vars lead to warning)
  check_ct_data(data, spec) |>
     expect_equal(data) |>
     expect_warning(regexp = "Values not permitted:.*18-64 years.*65-80 years.*>80 years")

  # Check check_ct_data ran across entire dataset
  check_ct_data(adex, adex_spec) |>
     expect_equal(adex) |>
     expect_warning(regexp = "Codelist: PARAMCD EQ NUMINJM.*Invalid 1.*Invalid 2") |>
     expect_warning(regexp = "Values not permitted.*TABLET") |>
     expect_warning(regexp = "Values not permitted.*QW")

  # Check verbose parameter
  check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "message") |>
     expect_message(regexp = "All controlled terminology checks passed")

  expect_silent(check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "warn"))
  expect_silent(check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "w"))

  # Cannnot be "silent"
  check_ct_data(data, spec, omit_vars = c("AGEGR2", "AGEGR2N"), verbose = "silent") |>
     expect_message("All controlled terminology checks passed") |>
     expect_warning("Argument `verbose` cannot be.*silent")

  check_ct_data(data, spec, na_acceptable = c("DCSREAS", "COMP8FL", "BMIBLGR1"), verbose = "silent") |>
     expect_warning("Argument `verbose` cannot be.*silent") |>
     expect_warning("Invalid controlled terminology detected")

  # Check character vector input for na_acceptable:
  expect_warning(check_ct_data(data, spec, na_acceptable = c("DCSREAS", "COMP8FL", "BMIBLGR1")))
  expect_error(check_ct_data(data, spec, 1))

  # Check omit_vars contains variables not present in the data
  check_ct_data(data, spec, omit_vars = c("MISSING_A", "MISSING_B")) |>
     expect_equal(data) |>
     expect_warning(regexp = "Variables not present in the data.*MISSING_A.*MISSING_B") |>
     expect_warning(regexp = "Values not permitted:.*18-64 years.*65-80 years.*>80 years")
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
