
library(metacore)
library(tibble)
library(dplyr)
test_that("check_ct_col works correctly",{
   spec <- suppressWarnings(define_to_MetaCore(metacore_example("ADaM_define.xml"))) %>%
      select_dataset("ADSL")
   data <- haven::read_xpt(pkg_example("adsl.xpt"))
   # Check it works with a character col
   expect_equal(check_ct_col(data, spec, ARM), TRUE)
   # Check it works with a numeric col
   expect_equal(check_ct_col(data, spec, TRT01PN), TRUE)
   # Check it works when passes a string
   expect_equal(check_ct_col(data, spec, "TRT01PN"), TRUE)



   # Test permitted Values
   spec2 <- suppressWarnings(spec_to_metacore(metacore_example("p21_mock.xlsx")))
   expect_equal(check_ct_col(data, spec2, ARM), TRUE)

   #Test external dictionaries
   data2 <- tibble(AELLT = "Hello")
   expect_error(check_ct_col(data2, spec2, AELLT))

   #Test a column that isn't in the dataset
   expect_error(check_ct_col(data, spec2, AELLT))

   #Test NA acceptable
   expect_equal(check_ct_col(data, spec, DCSREAS), FALSE)
   expect_equal(check_ct_col(data, spec, DCSREAS, TRUE), TRUE)
   data_w_miss <- data %>%
      mutate(TRT01PN = if_else(row_number() == 3, NA_real_, TRT01PN))
   expect_equal(check_ct_col(data_w_miss, spec, TRT01PN), FALSE)
   expect_equal(check_ct_col(data_w_miss, spec, TRT01PN, TRUE), TRUE)
})

test_that("check_ct_data works correctly", {
   spec <- suppressWarnings(define_to_MetaCore(metacore_example("ADaM_define.xml"))) %>%
      select_dataset("ADSL")
   data <- haven::read_xpt(pkg_example("adsl.xpt"))
   expect_error(check_ct_data(data, spec))
   expect_equaal(check_ct_data(data, spec, TRUE), TRUE)

})
