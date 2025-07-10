
spec <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), quiet = TRUE)
load(metacore::metacore_example("pilot_ADaM.rda"))
spec2 <- metacore %>% select_dataset("ADSL")
dm <- haven::read_xpt(metatools_example("dm.xpt"))


test_that("create_subgrps", {
  expect_equal(
    create_subgrps(c(1:10), c("<2", "2-5", ">5")),
    c("<2", "2-5", "2-5", "2-5", "2-5", ">5", ">5", ">5", ">5", ">5")
  )

  expect_equal(
    create_subgrps(c(1:10), c("<=2", ">2-5", ">5")),
    c("<=2", "<=2", ">2-5", ">2-5", ">2-5", ">5", ">5", ">5", ">5", ">5")
  )

  expect_equal(
    create_subgrps(c(1:10), c("<2", "2-<5", ">=5")),
    c("<2", "2-<5", "2-<5", "2-<5", ">=5", ">=5", ">=5", ">=5", ">=5", ">=5")
  )


  expect_equal(
     create_subgrps(c(1:10, NA),  c("<2", "2-5", ">5")),
     c("<2", "2-5", "2-5", "2-5", "2-5", ">5", ">5", ">5", ">5", ">5", NA)
  )
})

test_that("create_var_from_codelist", {
  data <- tibble::tribble(
    ~USUBJID, ~VAR1, ~VAR2,
    1, "M", "Male",
    2, "F", "Female",
    3, "F", "Female",
    4, "U", "Unknown",
    5, "M", "Male",
  )
  manual_data <- data %>%
    mutate(SEX = VAR1)
  expect_equal(create_var_from_codelist(data, spec, VAR2, SEX), manual_data)
  expect_equal(create_var_from_codelist(data, spec, "VAR2", "SEX"), manual_data)
  manual_data2 <- data %>%
    mutate(SEX = VAR2)
  expect_equal(
    create_var_from_codelist(data, spec, VAR1, SEX, decode_to_code = FALSE),
    manual_data2
  )
  # Test numeric
  num_out <- dm %>%
     mutate(TRT01P = ARM) %>%
     select(TRT01P) %>%
     create_var_from_codelist(spec2, TRT01P, TRT01PN) %>%
     head() %>%
     pull(TRT01PN)
  expect_equal(num_out, c(0,  0, 81, 54, 81,0))

  # Test for Variable not in specs
  expect_error(create_var_from_codelist(data, spec, VAR2, FOO))
})

test_that("create_cat_var", {
  # Create manual dataset to check against
  man_dat <- tibble::tribble(
    ~AGEGR1,     ~n,
    "65-80",    172,
    "<65",       42,
    ">80",       92,
  )
  # Grouping col only
  auto_dat <- create_cat_var(dm, spec2, AGE, AGEGR1) %>%
    group_by(AGEGR1) %>%
    dplyr::summarise(n = dplyr::n())
  expect_equal(auto_dat, man_dat)
  # Grouping Column and Numeric Decode
  grp_num_dat <- create_cat_var(dm, spec2, AGE, AGEGR1, AGEGR1N)
  grp_num_dat %>%
    group_by(AGEGR1) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    expect_equal(auto_dat)
  grp_num_dat %>%
    pull(AGEGR1N) %>%
    unique() %>%
    expect_equal(c(1:3))
  # Test errors
  expect_error(create_cat_var(dm, spec, AGE, ARM))
})

test_that("convert_var_to_fct", {
  # Codelist variable
  convert_var_to_fct(dm, spec, SEX) %>%
    pull(SEX) %>%
    levels() %>%
    expect_equal(c("F", "M", "U"))
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
