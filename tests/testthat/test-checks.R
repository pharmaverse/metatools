
# Load data to use across tests
load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL")
data <- haven::read_xpt(metatools_example("adsl.xpt"))
mod_ds_vars <- spec$ds_vars %>%
   mutate(core = if_else(variable %in% c("TRT01PN", "DISCONFL"), "Required", core))
spec_mod <- metacore::metacore(spec$ds_spec, mod_ds_vars, spec$var_spec, spec$value_spec, spec$derivations, spec$codelist) %>%
   suppressWarnings()

test_that("get_bad_ct works correctly", {

   # test na_acceptable
   expect_equal(get_bad_ct(data, spec, "DISCONFL"), character(0))
   expect_equal(get_bad_ct(data, spec, "DISCONFL", TRUE), character(0))
   expect_equal(get_bad_ct(data, spec, "DISCONFL", FALSE), "")

   expect_equal(get_bad_ct(data, spec_mod, "DISCONFL"), "")
   expect_equal(get_bad_ct(data, spec_mod, "DISCONFL", TRUE), character(0))
   expect_equal(get_bad_ct(data, spec_mod, "DISCONFL", FALSE), "")

   data_na <- data %>%
      mutate(DISCONFL = if_else(dplyr::row_number() == 1, NA_character_, DISCONFL))
   expect_equal(get_bad_ct(data_na, spec_mod, "DISCONFL"), c(NA_character_, ""))

})

test_that("check_ct_col works correctly", {
   # Check it works with a character col
   expect_equal(check_ct_col(data, spec, ARM), data)
   # Check it works with a numeric col
   expect_equal(check_ct_col(data, spec, TRT01PN), data)
   # Check it works when passes a string
   expect_equal(check_ct_col(data, spec, "TRT01PN"), data)

   # Test permitted Values
   spec2 <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), quiet = TRUE)
   expect_equal(check_ct_col(data, spec2, ARM), data)

   # Test external dictionaries
   data2 <- tibble::tibble(AELLT = "Hello")
   expect_error(check_ct_col(data2, spec2, AELLT),
                "We currently don't have the ability to check against external libraries")

   # Test a column that isn't in the dataset
   expect_error(check_ct_col(data, spec2, AELLT),
                "AELLT not found in dataset. Please check and try again")

   # Test NA acceptable
   expect_error(check_ct_col(data, spec, DCSREAS, FALSE))

   expect_equal(check_ct_col(data, spec, DCSREAS, TRUE), data)
   data_w_miss <- data %>%
      mutate(TRT01PN = if_else(dplyr::row_number() == 3, NA_real_, TRT01PN))
   expect_error(check_ct_col(data_w_miss, spec, TRT01PN, FALSE))
   expect_equal(get_bad_ct(data_w_miss, spec, TRT01PN, FALSE), NA_real_)
   expect_equal(check_ct_col(data_w_miss, spec, TRT01PN, TRUE), data_w_miss)
   ### Test with  a required column ###
   # Required without missing
   expect_equal(check_ct_col(data, spec_mod, TRT01PN), data)
   # Required with missing
   expect_error(check_ct_col(data, spec_mod, DISCONFL))
   expect_equal(get_bad_ct(data, spec_mod, DISCONFL), "")
   expect_equal(check_ct_col(data, spec_mod, DISCONFL, TRUE), data)
})

test_that("check_ct_data works correctly", {
   # Checking error for multiple words in multiple columns
   data_multi_word <- data %>%
      mutate(TRT01P = case_when(dplyr::row_number() == 2 ~ "Hello",
                                dplyr::row_number() == 3 ~ "World",
                                TRUE ~ TRT01P),
             TRT01A = TRT01P)
   expect_error(check_ct_data(data_multi_word, spec))

   expect_error(check_ct_data(data, spec, FALSE))
   expect_equal(check_ct_data(data, spec), data)
   expect_equal(check_ct_data(data, spec, TRUE), data)
   expect_error(check_ct_data(data, metacore, TRUE),
                "DSRAEFL does not have a unique control term, consider spcificing a dataset")
   expect_error(check_ct_data(data, spec_mod))
   expect_equal(check_ct_data(data, spec_mod, TRUE), data)

   # Check character vector input for na_acceptable:
   expect_error(check_ct_data(data, spec, na_acceptable = c("DISCONFL", "DSRAEFL")))
   expect_error(check_ct_data(data, spec, 1))

   # Check omit_vars:
   expect_error(check_ct_data(data, spec, omit_vars = c("A", "B")))
   expect_error(check_ct_data(data, spec, FALSE, omit_vars = c("DISCONFL", "DSRAEFL")))
   expect_equal(check_ct_data(data, spec_mod, na_acceptable = NULL, omit_vars = "DISCONFL"), data)

})

test_that("variable_check works correctly", {
   expect_equal(check_variables(data, spec), data)
   data_miss <- data %>% select(-1)
   expect_error(check_variables(data_miss, spec))
   data_extra <- data %>% mutate(foo = "hello")
   expect_error(check_variables(data_extra, spec))
   data_mis_ex <- data_extra %>% select(-1)
   expect_error(check_variables(data_mis_ex, spec))
})

test_that("check_unique_keys works as expected", {
   #check requirement for subsetted metacore object or a dataset name
   expect_error(check_unique_keys(data, metacore))
   #check missing variable keys error
   expect_error(check_unique_keys(data, metacore, dataset_name = "ADVS"))
   #check works correctly when records are unique
   expect_message(check_unique_keys(data, metacore, dataset_name = "ADSL"))
   #check works correctly when records are not unique
   test <- build_from_derived(metacore,
                              dataset_name = "ADLBHY",
                              ds_list = list("LB" = safetyData::sdtm_lb,
                                              "ADSL" = safetyData::adam_adsl,
                                              "ADLBC" = safetyData::adam_adlbc),
                              predecessor_only = FALSE,
                              keep = FALSE)
   expect_error(check_unique_keys(test, metacore, dataset_name = "ADLBHY"))
})
