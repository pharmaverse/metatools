library(haven)
library(dplyr)
library(stringr)
test_that("drop_unspec_vars", {
  metacore <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
    select_dataset("ADSL")
  data <- read_xpt(metatools_example("adsl.xpt")) %>%
    mutate(foo = "Hello", foo2 = "world")

  man_vars <- metacore$ds_vars %>%
    filter(dataset == "ADSL") %>%
    pull(.data$variable)
  man_dat <- data %>%
    select(man_vars)
  drop_unspec_vars(data, metacore) %>%
    expect_equal(man_dat)
})


test_that("build_from_derived", {
  metacore <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
    select_dataset("ADSL")
  ds_list <- list(DM = read_xpt(metatools_example("dm.xpt")))
  # Vars that should be brought through
  man_vars <- metacore$derivations %>%
    filter(str_detect(derivation, "^DM\\.")) %>%
    pull(derivation) %>%
    str_remove("^DM\\.") %>%
    unique() %>%
    ifelse(. == "ARM", "TRT01P", .) %>%
    sort()
  build_from_derived(metacore, ds_list, keep = FALSE) %>%
    names() %>%
    sort() %>%
    expect_equal(man_vars)

  # Vars pulled through with old columns kept
  man_vars <- metacore$derivations %>%
     filter(str_detect(derivation, "^DM\\.")) %>%
     pull(derivation) %>%
     str_remove("^DM\\.") %>%
     unique() %>%
     c(., "TRT01P") %>%
     sort()
  build_from_derived(metacore, ds_list) %>%
     names() %>%
     sort() %>%
     expect_equal(man_vars)
})
