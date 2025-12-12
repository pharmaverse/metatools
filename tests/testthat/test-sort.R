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

test_that("order_cols works with deprecated dataset_name parameter", {
  # Test that deprecated parameter still functions correctly
  result <- suppressMessages(suppressWarnings(
    order_cols(data, metacore, dataset_name = "ADSL")
  ))

  # Verify it produces same result as non-deprecated approach
  expected <- order_cols(data, spec)
  expect_equal(result, expected)
})

test_that("sort_by_key works with deprecated dataset_name parameter", {
  # Test that deprecated parameter still functions correctly
  shuffled_data <- data %>% arrange(TRT01P, AGE)

  result <- suppressMessages(suppressWarnings(
    sort_by_key(shuffled_data, metacore, dataset_name = "ADSL")
  ))

  # Verify it produces same result as non-deprecated approach
  expected <- sort_by_key(shuffled_data, spec)
  expect_equal(result, expected)
})
