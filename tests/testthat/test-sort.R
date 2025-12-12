# Suppress cli output during testing
options(cli.default_handler = function(...) {})

load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
data <- haven::read_xpt(metatools_example("adsl.xpt"))

test_that("order_cols reorders columns to match spec", {
  # Verify columns are reordered to match spec
  data %>%
    select(AGE, SITEID, everything()) %>%
    order_cols(spec) %>%
    expect_equal(data)
    
  # Should handle missing columns from spec
  data %>%
    select(AGE, everything(), -SITEID) %>%
    order_cols(spec) %>%
    expect_equal(select(data, -SITEID))

  # Should handle extra columns not in spec
  data %>%
    select(AGE, SITEID, everything()) %>%
    mutate(foo = "game") %>%
    order_cols(spec) %>%
    expect_equal(mutate(data, foo = "game"))
})

test_that("sort_by_key sorts data by key variables", {
  # Verify unsorted data is sorted to match original
  data %>%
    arrange(TRT01P, AGE) %>%
    sort_by_key(spec) %>%
    expect_equal(data)
})

test_that("order_cols warns about deprecated dataset_name parameter", {
  library(metacore)
  library(haven)

  load(metacore_example("pilot_ADaM.rda"))
  data <- read_xpt(metatools_example("adsl.xpt"))

  # Test that using dataset_name triggers deprecation warning
  lifecycle::expect_deprecated(
    order_cols(data, metacore, dataset_name = "ADSL")
  )
})

test_that("order_cols still works with deprecated dataset_name parameter", {
  library(metacore)
  library(haven)

  load(metacore_example("pilot_ADaM.rda"))
  data <- read_xpt(metatools_example("adsl.xpt"))

  # Suppress warning to test functionality
  suppressWarnings({
    result <- order_cols(data, metacore, dataset_name = "ADSL")
  })

  expect_s3_class(result, "data.frame")
})

test_that("sort_by_key warns about deprecated dataset_name parameter", {
  library(metacore)
  library(haven)

  load(metacore_example("pilot_ADaM.rda"))
  data <- read_xpt(metatools_example("adsl.xpt"))

  lifecycle::expect_deprecated(
    sort_by_key(data, metacore, dataset_name = "ADSL")
  )
})

test_that("sort_by_key still works with deprecated dataset_name parameter", {
  library(metacore)
  library(haven)

  load(metacore_example("pilot_ADaM.rda"))
  data <- read_xpt(metatools_example("adsl.xpt"))

  suppressWarnings({
    result <- sort_by_key(data, metacore, dataset_name = "ADSL")
  })

  expect_s3_class(result, "data.frame")
})
