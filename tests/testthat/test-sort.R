library(haven)

spec <- define_to_metacore(metacore_example("ADaM_define.xml"), quiet = TRUE) %>%
   select_dataset("ADSL")
data <- read_xpt(metatools_example("adsl.xpt"))
test_that("sort_order", {
   data %>%
      select(AGE, SITEID, everything()) %>%
      order_cols(spec) %>%
      expect_equal(data)
})

test_that("sort_key", {
   data %>%
      arrange(TRT01P, AGE) %>%
      sort_by_key(spec) %>%
      expect_equal(data)
})
