
load(metacore::metacore_example("pilot_ADaM.rda"))
spec <- metacore %>% select_dataset("ADSL")
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
