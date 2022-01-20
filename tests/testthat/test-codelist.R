library(haven)
library(tibble)
library(dplyr)

test_that("create_subgrps", {
   expect_equal(create_subgrps(c(1:10), c("<2", "2-5", ">5")),
                c("<2", "2-5", "2-5", "2-5", "2-5", ">5", ">5", ">5", ">5", ">5"))

   expect_equal(create_subgrps(c(1:10), c("<=2", ">2-5", ">5")),
                c("<=2", "<=2", ">2-5", ">2-5", ">2-5", ">5", ">5", ">5", ">5", ">5"))

   expect_equal(create_subgrps(c(1:10),c("<2", "2-<5", ">=5")),
                c("<2", "2-<5", "2-<5", "2-<5", ">=5", ">=5", ">=5", ">=5", ">=5", ">=5"))
})

test_that("create_var_from_codelist",{
   data <- tribble(
      ~USUBJID, ~VAR1,  ~VAR2,
      1,         "M",    "Male",
      2,         "F",    "Female",
      3,         "F",    "Female",
      4,         "U",    "Unknown",
      5,         "M",    "Male",
   )
   spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
   manual_data <- data %>%
      mutate(SEX = VAR1)
   expect_equal(create_var_from_codelist(data, spec, VAR2, SEX), manual_data)
   expect_equal(create_var_from_codelist(data, spec, "VAR2", "SEX"), manual_data)
   manual_data2 <- data %>%
      mutate(SEX = VAR2)
   expect_equal(
      create_var_from_codelist(data, spec,  VAR1, SEX, decode_to_code = FALSE),
      manual_data2)
   #Test for Variable not in specs
   expect_error(create_var_from_codelist(data, spec, VAR2, FOO))
})

test_that("create_cat_var", {
   spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
    select_dataset("ADSL")
   dm <- read_xpt(metatools_example("dm.xpt"))
   # Create manual dataset to check against
   man_dat <- tribble(
      ~AGEGR1,     ~n,
      "<65",       42,
      ">80",       92,
      "65-80",    172
   )
   # Grouping col only
   auto_dat <- create_cat_var(dm, spec, AGE, AGEGR1) %>%
      group_by(AGEGR1) %>%
      summarise(n = n())
   expect_equal(man_dat, auto_dat)
   # Grouping Column and Numeric Decode
   grp_num_dat <- create_cat_var(dm, spec, AGE, AGEGR1, AGEGR1N)
   grp_num_dat %>%
      group_by(AGEGR1) %>%
      summarise(n = n()) %>%
      expect_equal(auto_dat)
   grp_num_dat %>%
      pull(AGEGR1N) %>%
      unique() %>%
      expect_equal(as.character(1:3))
   # Test errors
   spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
   expect_error(create_cat_var(dm, spec, AGE, ARM))
})

test_that("convert_var_to_fct", {
   spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
   foo<-spec$codelist
   foo2<-spec$value_spec
   dm <- read_xpt(metatools_example("dm.xpt"))
   #Codelist variable
   convert_var_to_fct(dm, spec, SEX) %>%
      pull(SEX) %>%
      levels() %>%
      expect_equal(c("F","M","U"))
   # Param list variable
   convert_var_to_fct(dm, spec, ARM) %>%
      pull(ARM) %>%
      levels() %>%
      expect_equal(c("Screen Failure", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
   # Errors

   # Note although AELLT isn't in DM it will fail because it is an
   # external lib before it fails for not being the dataset
   expect_error(convert_var_to_fct(dm, spec, AELLT))
   expect_error(convert_var_to_fct(dm, spec, FOO))
})


