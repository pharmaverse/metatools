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
