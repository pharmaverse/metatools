# order_cols() tests ----
test_that("order_cols reorders columns to match metacore spec", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
  data <- haven::read_xpt(metatools_example("adsl.xpt"))

  result <- data %>%
    select(AGE, SITEID, everything()) %>%
    order_cols(spec)

  expect_equal(result, data)
})

test_that("order_cols handles extra columns not in spec", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
  data <- haven::read_xpt(metatools_example("adsl.xpt"))

  result <- data %>%
    select(AGE, everything(), -SITEID) %>%
    order_cols(spec)

  expected <- select(data, -SITEID)
  expect_equal(result, expected)
})

test_that("order_cols handles additional columns beyond spec", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
  data <- haven::read_xpt(metatools_example("adsl.xpt"))

  result <- data %>%
    select(AGE, SITEID, everything()) %>%
    mutate(foo = "game") %>%
    order_cols(spec)

  expected <- mutate(data, foo = "game")
  expect_equal(result, expected)
})

# sort_by_key() tests ----
test_that("sort_by_key sorts data by key variables from metacore spec", {
  load(metacore::metacore_example("pilot_ADaM.rda"))
  spec <- metacore %>% select_dataset("ADSL", quiet = TRUE)
  data <- haven::read_xpt(metatools_example("adsl.xpt"))

  result <- data %>%
    arrange(TRT01P, AGE) %>%
    sort_by_key(spec)

  expect_equal(result, data)
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
