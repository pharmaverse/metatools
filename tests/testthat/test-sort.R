# Suppress cli output during testing
options(cli.default_handler = function(...) { })

load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
data <- haven::read_xpt(metatools_example("adsl.xpt"))
test_that("sort_order", {
  data %>%
    select(AGE, SITEID, everything()) %>%
    order_cols(spec) %>%
    expect_equal(data)
   # Check when too many columns
   data %>%
      select(AGE, everything(), -SITEID) %>%
      order_cols(spec) %>%
      expect_equal(select(data, -SITEID))

   # Check when there are too few columns
   data %>%
      select(AGE, SITEID, everything()) %>%
      mutate(foo = "game") %>%
      order_cols(spec) %>%
      expect_equal(mutate(data, foo = "game"))

})

test_that("sort_key", {
  data %>%
    arrange(TRT01P, AGE) %>%
    sort_by_key(spec) %>%
    expect_equal(data)
})

test_that("order_cols with deprecated dataset_name parameter", {
  # Test using deprecated dataset_name parameter
  expect_warning(
    order_cols(data, metacore, dataset_name = "ADSL"),
    "was deprecated in metatools 0.2.0"
  )
})

test_that("sort_by_key with deprecated dataset_name parameter", {
  # Test using deprecated dataset_name parameter
  expect_warning(
    sort_by_key(data, metacore, dataset_name = "ADSL"),
    "was deprecated in metatools 0.2.0"
  )
})

test_that("order_cols with empty data frame", {
  empty_data <- data[0, ]
  result <- order_cols(empty_data, spec)
  expect_equal(nrow(result), 0)
  expect_true(all(names(data) %in% names(result)))
})

test_that("sort_by_key with empty data frame", {
  empty_data <- data[0, ]
  result <- sort_by_key(empty_data, spec)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(empty_data))
})

test_that("order_cols with data having only some ordered variables", {
  # Test with subset of variables
  subset_data <- data %>% select(STUDYID, USUBJID, AGE, SEX)
  result <- order_cols(subset_data, spec)
  expect_equal(nrow(result), nrow(subset_data))
  # Check that variables are reordered according to spec
  expect_true(all(names(subset_data) %in% names(result)))
})

test_that("sort_by_key with single row", {
  single_row <- data[1, ]
  result <- sort_by_key(single_row, spec)
  expect_equal(result, single_row)
})
