library(haven)
library(dplyr)
library(stringr)
load(metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL")
test_that("drop_unspec_vars", {
  data <- read_xpt(metatools_example("adsl.xpt")) %>%
    mutate(foo = "Hello", foo2 = "world")

  man_vars <- metacore$ds_vars %>%
    filter(dataset == "ADSL") %>%
    pull(.data$variable)
  man_dat <- data %>%
    select(all_of(man_vars))
  drop_unspec_vars(data, spec) %>%
    expect_equal(man_dat)
})


test_that("build_from_derived", {
  ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))

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
})



test_that("add_variables", {
   load(metacore_example("pilot_ADaM.rda"))
   spec <- metacore %>% select_dataset("ADSL")
   data <- read_xpt(metatools_example("adsl.xpt"))
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
